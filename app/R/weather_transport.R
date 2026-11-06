transport_mode <- function(feed, replacement = rep(FALSE, length(feed))) {
  result <- ifelse(feed == "tram", "tram", ifelse(
    feed %in% c("train", "regional_train", "interstate"), "rail", "bus"))
  result[!is.na(replacement) & replacement] <- "bus"
  result
}

load_weather_transport <- function(project, data_dir) {
  project$weather <- read_selected_csv(file.path(data_dir, "weather_for_crowd_join_2025.csv"), c(
    weather_location_id = "character", sensing_date = "character", hourday = "integer",
    weather_join_status = "character",
    weather_class = "character", weather_class_status = "character"
  ))
  project$weather_key <- paste(project$weather$sensing_date, project$weather$hourday)
  if (anyDuplicated(project$weather_key) ||
      !all(project$weather$weather_location_id == "MEL_CBD")) stop("天气查找键或区域不符合预期。")

  stops <- read_selected_csv(file.path(data_dir, "transport_stops_2025.csv"), c(
    stop_uid = "character", feed_mode = "character", stop_id = "character",
    stop_name = "character", latitude = "numeric", longitude = "numeric",
    record_type = "character", parent_station = "character", platform_code = "character",
    rail_replacement_platform = "integer", spatial_status = "character", snapshot_date = "character"
  ))
  routes <- read_selected_csv(file.path(data_dir, "transport_routes_2025.csv"), c(
    route_uid = "character", feed_mode = "character", route_short_name = "character",
    route_long_name = "character", agency_id = "character",
    rail_replacement_route = "integer", snapshot_date = "character"
  ))
  links <- read_selected_csv(file.path(data_dir, "stop_route_links_2025.csv"), c(
    stop_uid = "character", route_uid = "character", direction_id = "character"
  ))
  if (anyDuplicated(stops$stop_uid) || anyDuplicated(routes$route_uid)) stop("交通主表 ID 重复。")
  if (!all(links$stop_uid %in% stops$stop_uid) || !all(links$route_uid %in% routes$route_uid)) {
    stop("站点—线路关联引用了未知 ID。")
  }
  source_routes <- routes
  source_links <- links
  replacement_stops <- (!is.na(stops$rail_replacement_platform) & stops$rail_replacement_platform == 1L) |
    grepl("replacement", stops$stop_name, ignore.case = TRUE)
  replacement_routes <- (!is.na(routes$rail_replacement_route) & routes$rail_replacement_route == 1L) |
    grepl("replacement", routes$route_long_name, ignore.case = TRUE)
  routes <- routes[!replacement_routes, , drop = FALSE]
  links <- links[links$route_uid %in% routes$route_uid &
    links$stop_uid %in% stops$stop_uid[!replacement_stops], , drop = FALSE]

  signatures <- vapply(routes$route_uid, function(id) {
    rows <- links[links$route_uid == id, , drop = FALSE]
    paste(sort(unique(paste(rows$direction_id, rows$stop_uid, sep = "@"))), collapse = "|")
  }, character(1))
  bus <- routes$feed_mode == "bus" & !is.na(routes$route_short_name) &
    !is.na(routes$route_long_name) & nzchar(signatures)
  group_key <- routes$route_uid
  group_key[bus] <- paste("bus", routes$route_short_name[bus], routes$route_long_name[bus],
    routes$agency_id[bus], signatures[bus], sep = "\034")
  groups <- split(routes$route_uid, group_key)
  canonical <- vapply(group_key, function(key) sort(groups[[key]])[1], character(1))
  route_aliases <- data.frame(source_route_uid = routes$route_uid,
    canonical_route_uid = unname(canonical), stringsAsFactors = FALSE)
  routes <- routes[match(unique(canonical), routes$route_uid), , drop = FALSE]
  links$route_uid <- route_aliases$canonical_route_uid[match(links$route_uid, route_aliases$source_route_uid)]
  links <- unique(links)

  directions <- unique(links[, c("route_uid", "direction_id"), drop = FALSE])
  routes <- merge(routes, directions, by = "route_uid", sort = FALSE)
  routes$source_route_uid <- routes$route_uid
  routes$direction_label <- ifelse(routes$direction_id == "0", "A", "B")
  routes$route_uid <- paste0(routes$source_route_uid, "::direction:", routes$direction_id)
  links$route_uid <- paste0(links$route_uid, "::direction:", links$direction_id)
  routes$mode <- transport_mode(routes$feed_mode, routes$rail_replacement_route == 1L)
  routes$badge <- ifelse(is.na(routes$route_short_name), routes$route_long_name, routes$route_short_name)
  routes$display_name <- ifelse(is.na(routes$route_long_name), routes$badge, routes$route_long_name)
  line_names <- unique(data.frame(source_route_uid = routes$source_route_uid,
    title = paste(c(tram = "Tram", bus = "Bus", rail = "Rail")[routes$mode],
      routes$badge, "·", routes$display_name), stringsAsFactors = FALSE))
  duplicate_line <- duplicated(line_names$title) | duplicated(line_names$title, fromLast = TRUE)
  line_index <- ave(seq_len(nrow(line_names)), line_names$title, FUN = seq_along)
  line_names$title[duplicate_line] <- paste0(line_names$title[duplicate_line],
    " (", line_index[duplicate_line], ")")
  routes$line_title <- line_names$title[match(routes$source_route_uid, line_names$source_route_uid)]
  routes$title <- paste(c(tram = "Tram", bus = "Bus", rail = "Rail")[routes$mode],
                        routes$badge, "· Direction", routes$direction_label, "·", routes$display_name)
  routes <- routes[order(match(routes$mode, c("tram", "bus", "rail")), routes$badge,
                         routes$route_uid), , drop = FALSE]
  duplicate_title <- duplicated(routes$title) | duplicated(routes$title, fromLast = TRUE)
  title_index <- ave(seq_len(nrow(routes)), routes$title, FUN = seq_along)
  routes$title[duplicate_title] <- paste0(routes$title[duplicate_title], " (",
                                         title_index[duplicate_title], ")")
  stops$mode <- transport_mode(stops$feed_mode, stops$rail_replacement_platform == 1L)
  eligible <- stops$record_type %in% c("boarding_stop_or_platform", "station_reference") &
    !replacement_stops
  records <- stops[eligible, , drop = FALSE]
  records$display_id <- records$stop_uid
  rail <- records$mode == "rail"
  station_id <- ifelse(!is.na(records$parent_station), records$parent_station, records$stop_id)
  records$display_id[rail] <- paste0("rail_station:", station_id[rail])
  groups <- split(seq_len(nrow(records)), records$display_id)
  displayed <- lapply(names(groups), function(id) {
    group <- records[groups[[id]], , drop = FALSE]
    references <- which(group$record_type == "station_reference")
    chosen <- if (length(references)) references[1] else 1L
    row <- group[chosen, , drop = FALSE]
    data.frame(display_id = id, name = row$stop_name, mode = row$mode,
      latitude = row$latitude, longitude = row$longitude,
      boundary = any(group$spatial_status == "boundary_review_required"),
      source_records = nrow(group), stringsAsFactors = FALSE)
  })
  displayed <- do.call(rbind, displayed)
  mapping <- records[, c("stop_uid", "display_id"), drop = FALSE]
  display_links <- unique(data.frame(
    display_id = mapping$display_id[match(links$stop_uid, mapping$stop_uid)],
    route_uid = links$route_uid, direction_id = links$direction_id, stringsAsFactors = FALSE
  ))
  display_links <- display_links[!is.na(display_links$display_id), , drop = FALSE]
  nearby_links <- read_selected_csv(file.path(data_dir, "attraction_transport_links_2025.csv"), c(
    attraction_id = "character", stop_uid = "character", distance_m = "numeric",
    within_300m = "integer", link_search_radius_m = "numeric"
  ))
  if (!all(nearby_links$attraction_id %in% project$attractions$attraction_id) ||
      !all(nearby_links$stop_uid %in% stops$stop_uid)) stop("景点—站点关联引用了未知 ID。")
  nearby_links$display_id <- mapping$display_id[match(nearby_links$stop_uid, mapping$stop_uid)]
  project$transport <- list(stops = displayed, routes = routes, links = display_links,
    source_stops = stops, source_links = source_links, source_routes = source_routes,
    route_aliases = route_aliases,
    stop_mapping = mapping, nearby_links = nearby_links,
    snapshot_date = unique(stops$snapshot_date))
  project
}

get_nearby_transport <- function(project, attraction_id, radius = 300) {
  if (!radius %in% c(300, 500, 1000)) stop("周边范围必须为 300、500 或 1000 米。")
  empty <- project$transport$stops[FALSE, , drop = FALSE]
  empty$distance_m <- numeric(0)
  if (is.null(attraction_id) || !attraction_id %in% project$attractions$attraction_id) return(empty)
  links <- project$transport$nearby_links
  within <- if (radius == 300) links$within_300m == 1L else links$distance_m <= radius
  links <- links[links$attraction_id == attraction_id & !is.na(links$display_id) &
                   !is.na(links$distance_m) & !is.na(within) & within, , drop = FALSE]
  if (!nrow(links)) return(empty)
  distances <- aggregate(distance_m ~ display_id, data = links, FUN = min)
  result <- project$transport$stops[match(distances$display_id, project$transport$stops$display_id), , drop = FALSE]
  result$distance_m <- distances$distance_m
  result[order(result$distance_m), , drop = FALSE]
}

get_weather_snapshot <- function(project, date, hour) {
  position <- match(paste(date, hour), project$weather_key)
  if (is.na(position)) stop("天气日期或小时不在数据范围内。")
  row <- project$weather[position, , drop = FALSE]
  row$available <- isTRUE(row$weather_join_status == "ok") &&
    isTRUE(row$weather_class_status == "classified") &&
    row$weather_class %in% c("good_weather", "bad_weather")
  row$class_key <- if (!row$available) "unknown" else
    if (row$weather_class == "good_weather") "good" else "bad"
  row$label <- c(good = "Good weather", bad = "Bad weather", unknown = "Not available")[[row$class_key]]
  row
}

weather_banner_ui <- function(row, date, hour) {
  div(class = "weather-content", `data-weather-date` = date, `data-weather-hour` = hour,
    div(class = "weather-type", tags$span(class = "weather-label", "Weather"),
      tags$strong(class = paste("weather-class", paste0("weather-", row$class_key)),
        tags$span(class = "weather-dot", `aria-hidden` = "true"), row$label)),
    div(class = "weather-period", tags$strong(sprintf("%02d:00–%02d:59", hour, hour)),
      tags$span(date))
  )
}

transport_browser_data <- function(project, icon_version = NULL) {
  transport <- project$transport
  routes <- unname(lapply(seq_len(nrow(transport$routes)), function(i) {
    row <- transport$routes[i, ]
    list(id = row$route_uid, mode = row$mode, badge = row$badge,
         name = row$display_name, title = row$title, direction = row$direction_label,
         patterns = study_route_patterns_for_route(transport, row),
         stopIds = unname(as.list(transport$links$display_id[transport$links$route_uid == row$route_uid])))
  }))
  stops <- unname(lapply(seq_len(nrow(transport$stops)), function(i) {
    row <- transport$stops[i, ]
    list(id = row$display_id, name = row$name, mode = row$mode,
         lat = row$latitude, lng = row$longitude, boundary = row$boundary,
         sourceRecords = row$source_records, insideStudy = row$inside_study,
         routeIds = unname(as.list(transport$links$route_uid[transport$links$display_id == row$display_id])))
  }))
  # SVG updates must not reuse an older browser-cached outline-only marker.
  icon_files <- file.path("www", "transport", paste0(c("bus", "tram", "rail"), ".svg"))
  list(stops = stops, routes = routes,
    basemapDisplayBoundary = transport$study_routes$basemap_display_boundary,
    transportIconVersion = if (is.null(icon_version)) paste(unname(tools::md5sum(icon_files)), collapse = "") else icon_version)
}
