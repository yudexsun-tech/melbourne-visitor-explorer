# Full metropolitan-bus shapes stay separate from the original FTZ stop inventory.
load_full_bus_routes <- function(project, data_dir) {
  read <- function(name, columns) read_selected_csv(file.path(data_dir, name), columns)
  routes <- read("bus_routes_full_2025.csv", c(route_uid = "character",
    rail_replacement_route = "integer", snapshot_date = "character"))
  patterns <- read("bus_route_patterns_2025.csv", c(pattern_uid = "character",
    route_uid = "character", direction_id = "character", shape_uid = "character",
    origin_stop_uid = "character", destination_stop_uid = "character",
    trip_headsign = "character", is_default_direction_pattern = "integer"))
  ordered_stops <- read("bus_route_pattern_stops_2025.csv", c(pattern_uid = "character",
    route_uid = "character", direction_id = "character", shape_uid = "character",
    stop_order = "integer", stop_uid = "character"))
  points <- read("bus_route_shape_points_2025.csv", c(shape_uid = "character",
    point_order = "integer", latitude = "numeric", longitude = "numeric"))
  stops <- read("bus_stops_full_routes_2025.csv", c(stop_uid = "character",
    stop_name = "character", latitude = "numeric", longitude = "numeric"))
  stopifnot(!anyDuplicated(routes$route_uid), !anyDuplicated(patterns$pattern_uid),
    !anyDuplicated(stops$stop_uid), !any(routes$rail_replacement_route == 1L),
    all(routes$snapshot_date == "2025-10-03"),
    all(patterns$route_uid %in% routes$route_uid),
    all(patterns$shape_uid %in% points$shape_uid),
    all(ordered_stops$pattern_uid %in% patterns$pattern_uid),
    all(ordered_stops$stop_uid %in% stops$stop_uid),
    !anyDuplicated(paste(ordered_stops$pattern_uid, ordered_stops$stop_order)),
    !anyDuplicated(paste(points$shape_uid, points$point_order)),
    all(is.finite(points$latitude)), all(is.finite(points$longitude)),
    all(is.finite(stops$latitude)), all(is.finite(stops$longitude)))
  shape_groups <- split(points, points$shape_uid)
  stop_groups <- split(ordered_stops, ordered_stops$pattern_uid)
  payloads <- lapply(seq_len(nrow(patterns)), function(i) {
    pattern <- patterns[i, ]
    shape <- shape_groups[[pattern$shape_uid]]
    sequence <- stop_groups[[pattern$pattern_uid]]
    stopifnot(nrow(shape) >= 2L, nrow(sequence) >= 2L,
      all(sequence$route_uid == pattern$route_uid),
      all(sequence$direction_id == pattern$direction_id),
      all(sequence$shape_uid == pattern$shape_uid))
    shape <- shape[order(shape$point_order), ]
    sequence <- sequence[order(sequence$stop_order), ]
    stopifnot(sequence$stop_uid[1] == pattern$origin_stop_uid,
      tail(sequence$stop_uid, 1) == pattern$destination_stop_uid)
    list(id = pattern$pattern_uid, sourceRouteId = pattern$route_uid,
      shapeId = pattern$shape_uid, directionId = pattern$direction_id,
      isDefault = pattern$is_default_direction_pattern == 1L,
      origin = stops$stop_name[match(pattern$origin_stop_uid, stops$stop_uid)],
      destination = stops$stop_name[match(pattern$destination_stop_uid, stops$stop_uid)],
      stopIds = unname(as.list(sequence$stop_uid)),
      latlngs = unname(lapply(seq_len(nrow(shape)), function(j)
        list(shape$latitude[j], shape$longitude[j]))))
  })
  names(payloads) <- patterns$pattern_uid
  project$transport$full_bus <- list(routes = routes, patterns = patterns,
    stops = stops, payloads = payloads)
  project
}

full_bus_patterns_for_route <- function(transport, route) {
  if (is.null(transport$full_bus) || route$mode != "bus") return(list())
  aliases <- transport$route_aliases
  sources <- aliases$source_route_uid[aliases$canonical_route_uid == route$source_route_uid]
  rows <- transport$full_bus$patterns
  rows <- rows[rows$route_uid %in% sources & rows$direction_id == route$direction_id, ]
  # Prefer the exact canonical source, then the data package's default. A stop hover
  # may choose another compatible pattern that actually serves that stop.
  rows <- rows[order(rows$route_uid != route$source_route_uid,
    -rows$is_default_direction_pattern, rows$pattern_uid), ]
  unname(transport$full_bus$payloads[rows$pattern_uid])
}

full_bus_stops_for_browser <- function(transport) {
  if (is.null(transport$full_bus)) return(list())
  rows <- transport$full_bus$stops
  unname(lapply(seq_len(nrow(rows)), function(i)
    list(id = rows$stop_uid[i], name = rows$stop_name[i],
      lat = rows$latitude[i], lng = rows$longitude[i])))
}
