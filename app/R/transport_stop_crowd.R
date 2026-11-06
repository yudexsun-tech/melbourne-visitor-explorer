# Extend the existing bus proxy to tram platforms and railway reference points.
load_transport_stop_crowd <- function(project, data_dir) {
  links <- read_selected_csv(file.path(data_dir, "tram_rail_pedestrian_sensor_links.csv"), c(
    display_id = "character", mode = "character", location_id = "character",
    distance_m = "numeric", max_distance_m = "numeric", proxy_status = "character",
    measurement_scope = "character"))
  stops <- project$transport$stops
  matched <- links$proxy_status == "nearby_street_sensor"
  stopifnot(!anyDuplicated(links$display_id),
    setequal(links$display_id, stops$display_id[stops$mode %in% c("tram", "rail")]),
    identical(links$mode, stops$mode[match(links$display_id, stops$display_id)]),
    all(links$location_id[matched] %in% project$sensor_heat$sensors$location_id),
    all(is.finite(links$distance_m[matched]) & links$distance_m[matched] >= 0 &
      links$distance_m[matched] <= links$max_distance_m[matched]),
    all(is.na(links$location_id[!matched]) & is.na(links$distance_m[!matched])),
    all(links$proxy_status %in% c("nearby_street_sensor", "no_sensor_within_500m")),
    all(links$measurement_scope == "nearby_street_passages_not_transport_passengers"))
  bus <- project$bus_stop_crowd
  project$transport_stop_crowd <- rbind(
    data.frame(display_id = bus$stop_uid, mode = "bus", location_id = bus$location_id,
      distance_m = bus$distance_m, proxy_status = bus$proxy_status),
    links[, c("display_id", "mode", "location_id", "distance_m", "proxy_status")])
  project
}

get_transport_stop_crowd_snapshot <- function(project, month, weather, day_type, hour) {
  rows <- project$sensor_heat$profiles[[profile_key(month, weather, day_type)]]
  stopifnot(!is.null(rows), length(hour) == 1L, !is.na(hour), hour %in% 0:23)
  rows <- rows[rows$hourday == hour, , drop = FALSE]
  result <- project$transport$stops[, c("display_id", "name", "mode", "inside_study"), drop = FALSE]
  links <- project$transport_stop_crowd
  linked <- match(result$display_id, links$display_id)
  result$location_id <- links$location_id[linked]
  result$sensor_distance_m <- links$distance_m[linked]
  result$proxy_status <- ifelse(is.na(linked), "no_original_sensor_link", links$proxy_status[linked])
  index <- match(result$location_id, rows$location_id)
  result$display_count <- rows$value[index]
  result$sample_days <- as.integer(ifelse(is.na(index), 0L, rows$samples[index]))
  result$effective_width_m <- rows$effective_width_m[index]
  result$flow_ppmm <- rows$flow_ppmm[index]
  result$color_key <- classify_street_flow(project, result$flow_ppmm)
  result
}

get_transport_stop_crowd_frame <- function(project, month, weather, day_type, hour) {
  rows <- get_transport_stop_crowd_snapshot(project, month, weather, day_type, hour)
  list(month = month, weather = weather, dayType = day_type, hour = as.integer(hour),
    points = unname(lapply(seq_len(nrow(rows)), function(i) list(
      id = rows$display_id[i], state = rows$color_key[i], value = rows$display_count[i],
      displayValue = format_count(round_crowd_count(rows$display_count[i])),
      flowPpmm = rows$flow_ppmm[i], effectiveWidthM = rows$effective_width_m[i],
      samples = rows$sample_days[i], sensorId = rows$location_id[i],
      sensorDistanceM = rows$sensor_distance_m[i], proxyStatus = rows$proxy_status[i]
    ))))
}
