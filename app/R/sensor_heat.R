# Heatmap-only calendar: attraction statistics and the shared UI remain unchanged.
sensor_heat_calendar <- function(project, data_dir) {
  weather <- read_selected_csv(file.path(data_dir, "weather_for_crowd_join_2024.csv"), c(
    sensing_date = "character", hourday = "integer", weather_class = "character",
    weather_join_status = "character", weather_class_status = "character"))
  holidays <- read_selected_csv(file.path(data_dir, "public_holidays_victoria_2024.csv"),
    c(sensing_date = "character"))
  stopifnot(!anyDuplicated(paste(weather$sensing_date, weather$hourday)),
            all(substr(weather$sensing_date, 1, 4) == "2024"))
  groups <- split(weather, weather$sensing_date)
  daily <- vapply(groups, function(rows) {
    rows <- rows[rows$hourday %in% 9:16, , drop = FALSE]
    if (nrow(rows) != 8L || anyNA(rows$weather_class) ||
        !isTRUE(all(rows$weather_join_status == "ok" &
                    rows$weather_class_status == "classified"))) return(NA_character_)
    if (sum(rows$weather_class == "good_weather") >= 6L) "good_weather" else "bad_weather"
  }, character(1))
  calendar <- data.frame(date = names(daily), stringsAsFactors = FALSE)
  calendar$month <- substr(calendar$date, 1, 7)
  calendar$weekday <- (as.integer(as.Date(calendar$date)) + 3L) %% 7L + 1L
  calendar$is_public_holiday <- calendar$date %in% holidays$sensing_date
  calendar$day_type <- ifelse(calendar$weekday >= 6L | calendar$is_public_holiday,
                             "holiday", "weekdays")
  calendar$weather <- unname(daily)
  calendar <- rbind(calendar, project$calendar)
  stopifnot(!anyDuplicated(calendar$date))
  calendar[order(calendar$date), , drop = FALSE]
}

load_sensor_heat <- function(project, data_dir) {
  calendar <- sensor_heat_calendar(project, data_dir)
  metadata <- lapply(c(2024L, 2025L), function(year) {
    read_selected_csv(file.path(data_dir, sprintf("pedestrian_sensors_ftz_%s.csv", year)),
      c(location_id = "character", latitude = "numeric", longitude = "numeric"))
  })
  # Preparation checks historical coordinates. Never silently move a sensor ID.
  sensors <- unique(do.call(rbind, metadata))
  if (anyDuplicated(sensors$location_id) || anyNA(sensors) ||
      any(!is.finite(sensors$latitude)) || any(!is.finite(sensors$longitude))) {
    stop("Heatmap sensor IDs require one verified coordinate pair across the input years.")
  }
  sensors <- sensors[order(as.numeric(sensors$location_id)), , drop = FALSE]
  widths <- read_selected_csv(file.path(data_dir, "sensor_effective_widths.csv"), c(
    location_id = "character", effective_width_m = "numeric", width_basis = "character"))
  stopifnot(!anyDuplicated(widths$location_id), setequal(widths$location_id, sensors$location_id),
    all(is.finite(widths$effective_width_m) & widths$effective_width_m > 0),
    all(widths$width_basis == "estimated_from_official_street_geometry"))
  sensors$effective_width_m <- widths$effective_width_m[match(sensors$location_id, widths$location_id)]
  records <- do.call(rbind, lapply(c(2024L, 2025L), function(year) {
    rows <- read_selected_csv(file.path(data_dir,
      sprintf("pedestrian_sensor_hourly_ftz_%s.csv", year)), c(
        location_id = "character", sensing_date = "character", hourday = "integer",
        pedestriancount = "numeric", source_observation_present = "integer",
        crowd_class_status = "character"))
    if (anyNA(rows$location_id) || anyNA(rows$sensing_date) || anyNA(rows$hourday) ||
        any(!rows$hourday %in% 0:23) || any(substr(rows$sensing_date, 1, 4) != year) ||
        anyDuplicated(paste(rows$location_id, rows$sensing_date, rows$hourday)) ||
        !all(rows$location_id %in% metadata[[match(year, c(2024L, 2025L))]]$location_id)) {
      stop("Invalid heatmap observation IDs, dates or duplicate hourly keys.")
    }
    rows
  }))
  day_index <- match(records$sensing_date, calendar$date)
  if (anyNA(day_index)) stop("Heatmap dates are missing from the weather/day-type calendar.")
  keep <- !is.na(records$source_observation_present) & records$source_observation_present == 1L &
    !is.na(records$crowd_class_status) & records$crowd_class_status == "classified" &
    is.finite(records$pedestriancount) & records$pedestriancount >= 0 &
    !is.na(calendar$weather[day_index])
  valid <- records[keep, , drop = FALSE]
  days <- calendar[day_index[keep], , drop = FALSE]
  # Keep the UI's existing YYYY-MM keys, pooling matching month numbers across years.
  month_keys <- stats::setNames(project$months, substr(project$months, 6, 7))
  valid$key <- profile_key(unname(month_keys[substr(days$date, 6, 7)]), days$weather, days$day_type)
  grouped <- split(valid, valid$key)
  profiles <- list(); dates_by_profile <- list()
  for (key in names(project$monthly_profiles)) {
    parts <- strsplit(key, "/", fixed = TRUE)[[1]]
    dates_by_profile[[key]] <- calendar$date[!is.na(calendar$weather) &
      substr(calendar$date, 6, 7) == substr(parts[1], 6, 7) &
      calendar$weather == parts[2] & calendar$day_type == parts[3]]
    grid <- expand.grid(location_id = sensors$location_id, hourday = 0:23,
                        stringsAsFactors = FALSE)
    grid$value <- NA_real_; grid$samples <- 0L
    grid$samples_2024 <- 0L; grid$samples_2025 <- 0L
    rows <- grouped[[key]]
    if (!is.null(rows) && nrow(rows)) {
      rows$n <- 1L
      rows$n2024 <- as.integer(substr(rows$sensing_date, 1, 4) == "2024")
      rows$n2025 <- 1L - rows$n2024
      totals <- aggregate(cbind(pedestriancount, n, n2024, n2025) ~ location_id + hourday,
                          rows, sum)
      index <- match(paste(grid$location_id, grid$hourday), paste(totals$location_id, totals$hourday))
      present <- !is.na(index)
      grid$value[present] <- totals$pedestriancount[index[present]] / totals$n[index[present]]
      grid$samples[present] <- as.integer(totals$n[index[present]])
      grid$samples_2024[present] <- as.integer(totals$n2024[index[present]])
      grid$samples_2025[present] <- as.integer(totals$n2025[index[present]])
    }
    grid$effective_width_m <- sensors$effective_width_m[match(grid$location_id, sensors$location_id)]
    # Keep raw hourly means intact for attractions, details and passage counts.
    grid$flow_ppmm <- grid$value / (60 * grid$effective_width_m)
    profiles[[key]] <- grid
  }
  project$sensor_heat <- list(sensors = sensors, profiles = profiles, calendar = calendar,
    dates = dates_by_profile, years = c(2024L, 2025L),
    scale = sensor_heat_display_scale(profiles), radius_m = 110,
    metric = "estimated_passages_per_minute_per_metre", busy_threshold_ppmm = 18)
  # Reuse exactly these eligible observations for attraction/detail statistics.
  project$analysis_sensor_records <- valid[valid$location_id %in% project$conditions$location_id,
    c("location_id", "sensing_date", "hourday", "pedestriancount",
      "source_observation_present", "crowd_class_status"), drop = FALSE]
  project
}

# Original heatmap contrast rule, now calibrated to the width-adjusted weights.
# One global scale preserves differences between filters and hours.
sensor_heat_display_scale <- function(profiles) {
  values <- unlist(lapply(profiles, function(rows) rows$flow_ppmm), use.names = FALSE)
  values <- values[is.finite(values)]
  if (length(values)) max(1, unname(stats::quantile(values, .95)) * 2) else 1
}

get_sensor_heat_frame <- function(project, month, weather, day_type, hour, enabled = TRUE) {
  heat <- project$sensor_heat
  rows <- heat$profiles[[profile_key(month, weather, day_type)]]
  stopifnot(!is.null(rows), length(hour) == 1L, !is.na(hour), hour %in% 0:23)
  rows <- rows[rows$hourday == hour, , drop = FALSE]
  sensors <- heat$sensors
  index <- match(sensors$location_id, rows$location_id)
  points <- unname(lapply(seq_len(nrow(sensors)), function(i) list(
    id = sensors$location_id[i], lat = sensors$latitude[i], lng = sensors$longitude[i],
    value = rows$value[index[i]], samples = rows$samples[index[i]],
    flowPpmm = rows$flow_ppmm[index[i]], effectiveWidthM = sensors$effective_width_m[i]
  )))
  list(month = month, weather = weather, dayType = day_type, hour = as.integer(hour),
       enabled = isTRUE(enabled), metric = heat$metric, scale = heat$scale,
       radiusM = heat$radius_m, points = points)
}

# Transport markers represent the same nearby street metric as the heatmap.
classify_street_flow <- function(project, flow_ppmm) {
  ifelse(!is.finite(flow_ppmm), "unknown",
    ifelse(flow_ppmm >= project$sensor_heat$busy_threshold_ppmm, "crowded", "quiet"))
}
