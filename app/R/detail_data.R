detail_read_csv <- function(path, numeric_fields = character()) {
  if (!file.exists(path)) stop("详情数据文件不存在：", path)
  result <- read.csv(path, colClasses = "character", check.names = FALSE,
    fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  for (field in intersect(numeric_fields, names(result))) {
    value <- suppressWarnings(as.numeric(result[[field]]))
    if (any(!is.na(result[[field]]) & is.na(value))) stop("详情数值字段无法转换：", field)
    result[[field]] <- value
  }
  result
}

load_attraction_detail_data <- function(project, data_dir = "data") {
  read <- function(name, numeric_fields = character()) {
    detail_read_csv(file.path(data_dir, name), numeric_fields)
  }
  attractions <- read("attractions_2025.csv", c("latitude", "longitude", "boundary_distance_m"))
  descriptions <- read("attraction_details_2025.csv")
  access_points <- read("attraction_access_points_2025.csv",
    c("latitude", "longitude", "is_analysis_reference", "boundary_distance_m"))
  facilities <- read("facilities_2025.csv",
    c("latitude", "longitude", "boundary_distance_m", "street_reference_distance_m"))
  facility_links <- read("attraction_facility_links_2025.csv",
    c("distance_m", "distance_rank", "within_300m", "link_search_radius_m"))
  crowd_profiles <- read("attraction_crowd_profiles_2025.csv", c(
    "weekday_iso", "hourday", "median_pedestrian_count", "mean_pedestrian_count",
    "p25_pedestrian_count", "p75_pedestrian_count", "n_observed", "observed_fraction",
    "sensor_distance_m", "shared_sensor_attraction_count", "crowded_threshold_per_hour"))
  pathways <- read("station_pathways_2025.csv", c("from_latitude", "from_longitude",
    "to_latitude", "to_longitude", "traversal_time_seconds_raw"))
  transport_stops <- read("transport_stops_2025.csv", c("latitude", "longitude"))
  weather_fields <- c("hourday", "temperature_2m_c",
    "apparent_temperature_c", "relative_humidity_pct", "cloud_cover_pct", "precipitation_mm",
    "rain_mm", "wind_speed_10m_kmh", "wind_gusts_10m_kmh", "sunshine_duration_seconds", "is_day")
  weather <- do.call(rbind, lapply(c(2024L, 2025L), function(year)
    read(sprintf("weather_for_crowd_join_%s.csv", year), weather_fields)))
  stopifnot(!anyDuplicated(paste(weather$sensing_date, weather$hourday)))
  sources <- read("sources.csv")
  added_sources <- detail_read_csv(file.path(data_dir, "..", "metadata", "heatmap_sources.csv"))
  for (i in seq_len(nrow(added_sources))) {
    if (added_sources$source_id[i] %in% sources$source_id) next
    row <- sources[NA_integer_, , drop = FALSE]
    row$source_id <- added_sources$source_id[i]
    row$name <- paste(added_sources$publisher[i], "·", added_sources$data_period[i])
    row$publisher <- added_sources$publisher[i]
    row$source_url <- added_sources$url[i]
    row$data_date <- added_sources$data_period[i]
    row$processing <- added_sources$purpose[i]
    row$attribution <- added_sources$attribution[i]
    row$retrieved_date <- added_sources$retrieved_date[i]
    sources <- rbind(sources, row)
  }

  id_sets <- list(attractions$attraction_id, descriptions$attraction_id,
    facilities$facility_uid, access_points$access_point_uid, transport_stops$stop_uid)
  if (any(vapply(id_sets, anyDuplicated, integer(1)) > 0L)) stop("详情主表 ID 重复。")
  if (!setequal(descriptions$attraction_id, attractions$attraction_id) ||
      !setequal(attractions$attraction_id, project$attractions$attraction_id)) {
    stop("详情景点与地图景点 ID 不一致。")
  }
  if (!all(facility_links$attraction_id %in% attractions$attraction_id) ||
      !all(facility_links$facility_uid %in% facilities$facility_uid) ||
      !all(access_points$attraction_id %in% attractions$attraction_id)) {
    stop("详情关联引用了未知景点或设施。")
  }
  if (anyDuplicated(paste(facility_links$attraction_id, facility_links$facility_uid)) ||
      anyDuplicated(paste(crowd_profiles$attraction_id, crowd_profiles$weekday_iso, crowd_profiles$hourday))) {
    stop("详情关联或典型人流键重复。")
  }
  if (!all(crowd_profiles$attraction_id %in% attractions$attraction_id) ||
      anyNA(crowd_profiles$hourday) || !all(crowd_profiles$hourday %in% 0:23) ||
      anyNA(crowd_profiles$weekday_iso) || !all(crowd_profiles$weekday_iso %in% 1:7)) {
    stop("典型人流中的景点、星期或小时不符合预期。")
  }
  facilities$inside_study <- inside_study_boundary(project, facilities$longitude, facilities$latitude)
  facilities$access_restricted <- !is.na(facilities$access_raw) &
    tolower(facilities$access_raw) %in% c("private", "no", "customers")
  access_points$inside_study <- inside_study_boundary(project, access_points$longitude, access_points$latitude)
  project$detail <- list(attractions = attractions, descriptions = descriptions,
    access_points = access_points, facilities = facilities, facility_links = facility_links,
    crowd_profiles = crowd_profiles, pathways = pathways, transport_stops = transport_stops,
    weather = weather, sources = sources,
    condition_index = split(seq_len(nrow(project$conditions)), project$conditions$attraction_id))
  project
}

detail_facilities <- function(project, id, radius = 300,
    types = c("toilet", "drinking_water", "bench", "shelter"),
    needs = character(), inside_only = TRUE) {
  if (length(radius) != 1L || !is.finite(radius) || radius < 0 || radius > 1000) {
    stop("设施范围必须为 0–1000 米；数据关联最多覆盖 1000 米。")
  }
  supported <- c("wheelchair", "changing_table", "free", "backrest")
  if (length(setdiff(needs, supported))) stop("未知设施属性筛选：", paste(setdiff(needs, supported), collapse = ", "))
  detail <- project$detail
  result <- detail$facilities[FALSE, , drop = FALSE]
  result$distance_m <- numeric(0)
  result$distance_rank <- numeric(0)
  result$display_name <- character(0)
  result$facility_number <- integer(0)
  if (length(id) != 1L || is.na(id) || !id %in% detail$attractions$attraction_id) return(result)
  links <- detail$facility_links
  links <- links[links$attraction_id == id & !is.na(links$distance_m) &
    links$distance_m <= 1000, , drop = FALSE]
  links <- links[order(links$distance_m, links$facility_uid), , drop = FALSE]
  links$facility_number <- seq_len(nrow(links))
  links <- links[links$distance_m <= radius, , drop = FALSE]
  if (!nrow(links) || !length(types)) return(result)
  result <- detail$facilities[match(links$facility_uid, detail$facilities$facility_uid), , drop = FALSE]
  result$distance_m <- links$distance_m
  result$distance_rank <- links$distance_rank
  result$facility_number <- links$facility_number
  keep <- !is.na(result$facility_type) & result$facility_type %in% types
  if (isTRUE(inside_only)) keep <- keep & !is.na(result$inside_study) & result$inside_study
  for (need in needs) {
    field <- switch(need, wheelchair = "wheelchair_raw", changing_table = "changing_table_raw",
      free = "fee_raw", backrest = "backrest_raw")
    accepted <- switch(need, wheelchair = c("yes", "designated"), free = "no", "yes")
    keep <- keep & !is.na(result[[field]]) & tolower(result[[field]]) %in% accepted
  }
  result <- result[keep, , drop = FALSE]
  labels <- c(toilet = "Toilet", drinking_water = "Drinking water", bench = "Bench", shelter = "Shelter")
  unnamed <- is.na(result$name) | !nzchar(trimws(result$name))
  result$display_name <- result$name
  result$display_name[unnamed] <- unname(labels[result$facility_type[unnamed]])
  result <- result[order(result$distance_m, result$facility_uid), , drop = FALSE]
  rownames(result) <- NULL
  result
}

detail_transport_groups <- function(stops) {
  stops <- stops[order(stops$distance_m, stops$display_id, na.last = TRUE), , drop = FALSE]
  key <- tolower(gsub("[[:space:]]+", " ", trimws(stops$name)))
  unnamed <- is.na(key) | !nzchar(key)
  key[unnamed] <- paste0("unnamed:", stops$display_id[unnamed])
  first <- which(!duplicated(key))
  result <- stops[first, , drop = FALSE]
  result$member_display_ids <- I(lapply(first, function(i) stops$display_id[key == key[i]]))
  rownames(result) <- NULL
  result
}

detail_hour_profile <- function(project, id, dates) {
  result <- data.frame(hour = 0:23, mean = NA_real_, p25 = NA_real_, p75 = NA_real_, n = 0L)
  if (length(id) != 1L || is.na(id) || !id %in% project$attractions$attraction_id) return(result)
  dates <- intersect(unique(as.character(dates)), project$dates)
  if (!length(dates)) return(result)
  index <- project$detail$condition_index[[id]]
  if (is.null(index)) index <- which(project$conditions$attraction_id == id)
  rows <- project$conditions[index, , drop = FALSE]
  valid <- rows$sensing_date %in% dates & !is.na(rows$crowd_class_status) &
    rows$crowd_class_status == "classified" & is.finite(rows$pedestriancount) & rows$pedestriancount >= 0
  rows <- rows[valid, , drop = FALSE]
  for (hour in 0:23) {
    values <- rows$pedestriancount[rows$hourday == hour]
    i <- hour + 1L
    result$n[i] <- length(values)
    if (length(values)) {
      # Match the heatmap's sum / valid-date count exactly, including binary rounding.
      result$mean[i] <- sum(values) / length(values)
      bounds <- stats::quantile(values, c(.25, .75), names = FALSE, type = 7)
      result$p25[i] <- bounds[1]
      result$p75[i] <- bounds[2]
    }
  }
  result
}

detail_typical_week <- function(project, id) {
  rows <- project$detail$crowd_profiles
  if (length(id) != 1L || is.na(id)) return(rows[FALSE, , drop = FALSE])
  rows <- rows[rows$attraction_id == id, , drop = FALSE]
  rows <- rows[order(rows$weekday_iso, rows$hourday), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# The weekly chart includes both day types for the selected month and weather.
detail_week_dates <- function(project, month, weather) {
  sort(unique(unlist(lapply(c("weekdays", "holiday"), function(day_type) {
    project$monthly_profiles[[profile_key(month, weather, day_type)]]$dates
  }), use.names = FALSE)))
}

# Counts use only observed dates; regular opening hours remain known without observations.
detail_week_profile <- function(project, id, dates) {
  if (length(id) != 1L || is.na(id) || !id %in% project$attractions$attraction_id)
    return(data.frame())
  calendar <- project$calendar
  calendar <- calendar[calendar$date %in% dates, , drop = FALSE]
  result <- do.call(rbind, lapply(1:7, function(day) {
    hours <- detail_hour_profile(project, id, calendar$date[calendar$weekday == day])
    data.frame(weekday_iso = day, weekday_name = c("Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday", "Sunday")[day], hourday = hours$hour,
      mean_pedestrian_count = hours$mean, p25_pedestrian_count = hours$p25,
      p75_pedestrian_count = hours$p75, n_observed = hours$n,
      crowd_class = unname(c(quiet = "Quiet", moderate = "Moderate", crowded = "Crowded", unknown = NA_character_)[
        classify_crowd_count(project, hours$mean, id)]),
      stringsAsFactors = FALSE)
  }))
  result$opening_status <- vapply(seq_len(nrow(result)), function(i) {
    day_dates <- calendar$date[calendar$weekday == result$weekday_iso[i]]
    if (length(day_dates)) attraction_opening_state(project, id, day_dates, result$hourday[i]) else
      attraction_weekday_opening_state(project, id, result$weekday_iso[i], result$hourday[i])
  }, character(1))
  result$display_class <- ifelse(result$crowd_class == "Crowded", "Busy", result$crowd_class)
  result$display_class[result$opening_status == "closed"] <- "Closed"
  result$display_class[result$opening_status == "unknown"] <- "Not available"
  mixed <- result$opening_status == "mixed" & !is.na(result$display_class)
  result$display_class[mixed] <- paste(result$display_class[mixed], "Hours vary by day", sep = " · ")
  rownames(result) <- NULL
  result
}
