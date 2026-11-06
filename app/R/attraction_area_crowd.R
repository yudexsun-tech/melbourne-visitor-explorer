# Area-normalised attraction classification; sensor counts and transit stay intact.
load_attraction_area_crowd <- function(project, data_dir) {
  config <- jsonlite::fromJSON(file.path(data_dir, "..", "metadata", "attraction_area_crowd_model.json"))
  scenarios <- read_selected_csv(file.path(data_dir, "attraction_capacity_scenarios.csv"), c(
    attraction_id = "character", effective_area_m2 = "numeric",
    space_per_person_m2_assumed = "numeric"))
  areas <- scenarios[scenarios$space_per_person_m2_assumed ==
    config$area_selection_space_per_person_m2, c("attraction_id", "effective_area_m2")]
  additional <- read_selected_csv(file.path(data_dir, config$supplementary_area_file), c(
    attraction_id = "character", effective_area_m2 = "numeric"))
  areas <- rbind(areas, additional)
  stopifnot(!anyDuplicated(areas$attraction_id),
    all(is.finite(areas$effective_area_m2) & areas$effective_area_m2 > 0),
    all(areas$attraction_id %in% project$attractions$attraction_id),
    setequal(setdiff(project$attractions$attraction_id, areas$attraction_id),
      config$fallback_attraction_ids),
    identical(config$calibration_method, "per_attraction_open_hours_terciles"),
    !is.null(project$opening_hours))
  project$attraction_area_crowd <- list(areas = areas, config = config)
  project <- calibrate_attraction_open_terciles(project)
  keys <- classify_crowd_count(project, project$conditions$pedestriancount,
    project$conditions$attraction_id)
  project$conditions$crowd_class <- unname(c(quiet = "Quiet", moderate = "Moderate", crowded = "Crowded",
    unknown = NA_character_)[keys])
  project
}

attraction_effective_area <- function(project, attraction_id) {
  areas <- project$attraction_area_crowd$areas
  areas$effective_area_m2[match(attraction_id, areas$attraction_id)]
}

attraction_flow_intensity <- function(project, count, attraction_id) {
  area <- attraction_effective_area(project, attraction_id)
  value <- count / area
  value[!is.finite(value) | value < 0] <- NA_real_
  value
}

# Calibrate once per attraction over all existing month/weather/day-type profiles.
# Only known-open observations contribute, including date-specific exceptions.
# Each finite hourly profile mean has equal weight; zero is a valid observation.
calibrate_attraction_open_terciles <- function(project) {
  samples <- lapply(project$attractions$attraction_id, function(id) {
    rows <- project$conditions[project$conditions$attraction_id == id, , drop = FALSE]
    valid <- !is.na(rows$crowd_class_status) & rows$crowd_class_status == "classified" &
      is.finite(rows$pedestriancount) & rows$pedestriancount >= 0
    rows <- rows[valid, , drop = FALSE]
    open <- rep(FALSE, nrow(rows))
    for (hour in 0:23) {
      index <- which(rows$hourday == hour)
      open[index] <- attraction_open_on_dates(project, id, rows$sensing_date[index], hour) %in% TRUE
    }
    rows <- rows[open, , drop = FALSE]
    do.call(rbind, lapply(names(project$monthly_profiles), function(key) {
      selected <- rows[rows$sensing_date %in% project$monthly_profiles[[key]]$dates, , drop = FALSE]
      if (!nrow(selected)) return(NULL)
      by_hour <- split(selected$pedestriancount, selected$hourday)
      count <- vapply(by_hour, function(value) sum(value) / length(value), numeric(1))
      data.frame(attraction_id = id, profile_key = key, hourday = as.integer(names(by_hour)),
        display_count = unname(count), sample_days = lengths(by_hour),
        flow_per_m2 = attraction_flow_intensity(project, unname(count), id),
        stringsAsFactors = FALSE, row.names = NULL)
    }))
  })
  calibration <- do.call(rbind, samples)
  rownames(calibration) <- NULL
  thresholds <- do.call(rbind, lapply(project$attractions$attraction_id, function(id) {
    values <- calibration$flow_per_m2[calibration$attraction_id == id]
    stopifnot(length(values) > 0L, all(is.finite(values) & values >= 0))
    cuts <- unname(stats::quantile(values, c(1/3, 2/3), type = 7))
    data.frame(attraction_id = id, moderate_threshold = cuts[1], busy_threshold = cuts[2],
      calibration_cells = length(values), quiet_cells = sum(values < cuts[1]),
      moderate_cells = sum(values >= cuts[1] & values < cuts[2]), busy_cells = sum(values >= cuts[2]),
      stringsAsFactors = FALSE)
  }))
  project$attraction_area_crowd$calibration <- calibration
  project$attraction_area_crowd$thresholds <- thresholds
  project
}

# Equivalent counts keep every chart's original passages/hour axis and values.
attraction_count_threshold <- function(project, attraction_id, band = c("busy", "moderate")) {
  band <- match.arg(band)
  model <- project$attraction_area_crowd
  index <- match(attraction_id, model$thresholds$attraction_id)
  threshold <- attraction_effective_area(project, attraction_id) *
    model$thresholds[[paste0(band, "_threshold")]][index]
  fallback <- attraction_id %in% model$config$fallback_attraction_ids
  threshold[fallback] <- if (band == "busy") project$crowded_threshold else NA_real_
  threshold
}
