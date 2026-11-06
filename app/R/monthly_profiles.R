profile_key <- function(month, weather, day_type) paste(month, weather, day_type, sep = "/")

load_monthly_profiles <- function(project, data_dir) {
  holidays <- read_selected_csv(file.path(data_dir, "public_holidays_victoria_2025.csv"),
    c(sensing_date = "character", holiday_name = "character"))
  calendar <- data.frame(date = project$dates, stringsAsFactors = FALSE)
  calendar$month <- substr(calendar$date, 1, 7)
  calendar$weekday <- (as.integer(as.Date(calendar$date)) + 3L) %% 7L + 1L
  calendar$is_public_holiday <- calendar$date %in% holidays$sensing_date
  calendar$day_type <- ifelse(calendar$weekday >= 6L | calendar$is_public_holiday,
                             "holiday", "weekdays")
  weather_days <- split(project$weather, project$weather$sensing_date)
  daily_class <- vapply(weather_days, function(rows) {
    rows <- rows[rows$hourday %in% 9:16, , drop = FALSE]
    if (nrow(rows) != 8L || anyNA(rows$weather_class) ||
        !all(rows$weather_join_status == "ok" & rows$weather_class_status == "classified")) return(NA_character_)
    if (sum(rows$weather_class == "good_weather") >= 6L) "good_weather" else "bad_weather"
  }, character(1))
  calendar$weather <- unname(daily_class[calendar$date])
  project$calendar <- calendar
  project$months <- sort(unique(calendar$month))
  month_number <- as.integer(substr(project$months, 6, 7))
  project$month_labels <- stats::setNames(
    month.name[month_number], project$months)
  rules <- read_selected_csv(file.path(data_dir, "crowd_classification_rules_2025.csv"),
    c(crowded_threshold_per_hour = "numeric"))
  project$crowded_threshold <- rules$crowded_threshold_per_hour[1]
  project$monthly_profiles <- list()
  # Only initialize calendar keys. Counts are built once from pooled sensor data.
  for (month in project$months) for (weather in c("good_weather", "bad_weather")) {
    for (day_type in c("holiday", "weekdays")) {
      project$monthly_profiles[[profile_key(month, weather, day_type)]] <- list()
    }
  }
  project
}

classify_crowd_count <- function(project, value, attraction_id = NULL) {
  threshold <- if (is.null(attraction_id)) project$crowded_threshold else
    attraction_count_threshold(project, attraction_id)
  moderate <- if (is.null(attraction_id)) NA_real_ else
    attraction_count_threshold(project, attraction_id, "moderate")
  ifelse(!is.finite(value) | value < 0 | !is.finite(threshold), "unknown",
    ifelse(value >= threshold, "crowded",
      ifelse(is.finite(moderate) & value >= moderate, "moderate", "quiet")))
}

get_monthly_snapshot <- function(project, month, weather, day_type, hour, apply_hours = TRUE) {
  profile <- project$monthly_profiles[[profile_key(month, weather, day_type)]]
  stopifnot(!is.null(profile), length(hour) == 1L, hour %in% 0:23)
  rows <- profile$rows[profile$rows$hourday == hour, , drop = FALSE]
  result <- project$attractions
  index <- match(result$attraction_id, rows$attraction_id)
  result$display_count <- rows$display_count[index]
  result$sample_days <- rows$sample_days[index]
  result$flow_per_m2 <- attraction_flow_intensity(project, result$display_count, result$attraction_id)
  result$color_key <- classify_crowd_count(project, result$display_count, result$attraction_id)
  result$status_zh <- unname(c(quiet = "Quiet", moderate = "Moderate", crowded = "Busy", unknown = "")[result$color_key])
  result$icon_url <- sprintf("icons/%s-%s.svg", result$icon_key, result$color_key)
  if (isTRUE(apply_hours)) result <- apply_attraction_opening_status(project, result, profile$dates, hour)
  result
}

# The original 2025 loader above initializes stable UI keys. After the heatmap
# is loaded, all displayed attraction statistics share its two-year calendar,
# exact primary-sensor means and eligible raw observations.
load_pooled_attraction_profiles <- function(project, data_dir) {
  mapping <- unique(project$conditions[, c("attraction_id", "location_id",
    "sensor_distance_m", "shared_sensor_attraction_count"), drop = FALSE])
  stopifnot(!anyDuplicated(mapping$attraction_id),
    setequal(mapping$attraction_id, project$attractions$attraction_id),
    all(mapping$location_id %in% project$sensor_heat$sensors$location_id))
  records <- project$analysis_sensor_records
  stopifnot(!is.null(records), !anyDuplicated(paste(records$location_id,
    records$sensing_date, records$hourday)))
  conditions <- merge(mapping, records, by = "location_id", sort = FALSE)
  conditions$crowd_class <- ifelse(conditions$pedestriancount >= project$crowded_threshold,
    "Crowded", "Quiet")
  conditions <- conditions[order(conditions$attraction_id, conditions$sensing_date,
    conditions$hourday), , drop = FALSE]
  rownames(conditions) <- NULL
  project$conditions <- conditions
  project$calendar <- project$sensor_heat$calendar
  project$dates <- project$calendar$date
  project$date_index <- split(seq_len(nrow(conditions)), conditions$sensing_date)
  project$attraction_primary_sensors <- mapping
  project$analysis_years <- project$sensor_heat$years
  project$analysis_year_label <- "2024–2025"
  for (key in names(project$monthly_profiles)) {
    grid <- expand.grid(attraction_id = project$attractions$attraction_id,
      hourday = 0:23, stringsAsFactors = FALSE)
    sensor_ids <- mapping$location_id[match(grid$attraction_id, mapping$attraction_id)]
    heat <- project$sensor_heat$profiles[[key]]
    index <- match(paste(sensor_ids, grid$hourday), paste(heat$location_id, heat$hourday))
    stopifnot(!anyNA(index))
    grid$display_count <- heat$value[index]
    grid$sample_days <- heat$samples[index]
    grid$samples_2024 <- heat$samples_2024[index]
    grid$samples_2025 <- heat$samples_2025[index]
    project$monthly_profiles[[key]] <- list(dates = project$sensor_heat$dates[[key]], rows = grid)
  }
  # Retain compatibility for date/hour weather queries, now for both years.
  weather24 <- read_selected_csv(file.path(data_dir, "weather_for_crowd_join_2024.csv"), c(
    weather_location_id = "character", sensing_date = "character", hourday = "integer",
    weather_join_status = "character", weather_class = "character", weather_class_status = "character"))
  project$weather <- rbind(weather24, project$weather)
  project$weather_key <- paste(project$weather$sensing_date, project$weather$hourday)
  stopifnot(!anyDuplicated(project$weather_key), !anyDuplicated(paste(conditions$attraction_id,
    conditions$sensing_date, conditions$hourday)))
  project
}

monthly_banner_ui <- function(project, month, weather, day_type, hour) {
  label <- if (weather == "good_weather") "Good weather" else "Bad weather"
  key <- if (weather == "good_weather") "good" else "bad"
  div(class = "weather-content",
    tags$h1(class = "main-map-title", "Melbourne CBD attraction map"),
    div(class = "weather-type", tags$span(class = "weather-label", "Weather"),
      tags$strong(class = paste("weather-class", paste0("weather-", key)),
        tags$span(class = "weather-dot", `aria-hidden` = "true"), label),
      tags$span(class = "weather-help",
        tags$button(type = "button", class = "weather-help-button",
          `aria-label` = "How good and bad weather are calculated",
          `aria-describedby` = "weather-help-text", "i"),
        tags$span(id = "weather-help-text", class = "weather-help-tooltip", role = "tooltip",
          "Good weather: at least 6 of 8 hours (9 am–5 pm) are mild (feels like 10–28°C), mostly dry, with no strong wind, fog or storms. Otherwise: Bad weather. Days with missing weather data are excluded.")))
  )
}
