# Bus-stop colours describe the original linked street sensor, not passengers.
load_bus_stop_crowd <- function(project, data_dir) {
  links <- read_selected_csv(file.path(data_dir, "bus_stop_pedestrian_sensor_links_2025.csv"), c(
    stop_uid = "character", location_id = "character", distance_m = "numeric",
    max_distance_m = "numeric", proxy_status = "character", measurement_scope = "character"))
  stopifnot(!anyNA(links), !anyDuplicated(links$stop_uid),
    all(links$stop_uid %in% project$transport$stops$display_id[project$transport$stops$mode == "bus"]),
    all(links$location_id %in% project$sensor_heat$sensors$location_id),
    all(is.finite(links$distance_m) & links$distance_m >= 0 & links$distance_m <= links$max_distance_m),
    all(links$proxy_status == "nearby_street_sensor"),
    all(links$measurement_scope == "nearby_street_passages_not_bus_passengers"))
  project$bus_stop_crowd <- links
  project
}

get_bus_stop_crowd_snapshot <- function(project, month, weather, day_type, hour) {
  rows <- project$sensor_heat$profiles[[profile_key(month, weather, day_type)]]
  stopifnot(!is.null(rows), length(hour) == 1L, !is.na(hour), hour %in% 0:23)
  rows <- rows[rows$hourday == hour, , drop = FALSE]
  stops <- project$transport$stops
  result <- stops[stops$mode == "bus", c("display_id", "name", "inside_study"), drop = FALSE]
  links <- project$bus_stop_crowd
  linked <- match(result$display_id, links$stop_uid)
  result$location_id <- links$location_id[linked]
  result$sensor_distance_m <- links$distance_m[linked]
  index <- match(result$location_id, rows$location_id)
  result$display_count <- rows$value[index]
  result$sample_days <- as.integer(ifelse(is.na(index), 0L, rows$samples[index]))
  result$effective_width_m <- rows$effective_width_m[index]
  result$flow_ppmm <- rows$flow_ppmm[index]
  result$color_key <- classify_street_flow(project, result$flow_ppmm)
  result
}

get_bus_stop_crowd_frame <- function(project, month, weather, day_type, hour) {
  rows <- get_bus_stop_crowd_snapshot(project, month, weather, day_type, hour)
  list(month = month, weather = weather, dayType = day_type, hour = as.integer(hour),
    points = unname(lapply(seq_len(nrow(rows)), function(i) list(
      id = rows$display_id[i], state = rows$color_key[i], value = rows$display_count[i],
      displayValue = format_count(round_crowd_count(rows$display_count[i])),
      flowPpmm = rows$flow_ppmm[i], effectiveWidthM = rows$effective_width_m[i],
      samples = rows$sample_days[i], sensorId = rows$location_id[i],
      sensorDistanceM = rows$sensor_distance_m[i]
    ))))
}
