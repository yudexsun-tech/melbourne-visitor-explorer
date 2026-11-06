# Region-limited shapes and direction labels from the same historical GTFS feeds.
load_study_transit_routes <- function(project, data_dir) {
  model <- jsonlite::fromJSON(file.path(data_dir, "study_transit_routes_2025.json"),
    simplifyVector = FALSE)
  stopifnot(identical(model$snapshotDate, "2025-10-03"))
  # Retain original GTFS paths between study-area stops, including outside
  # excursions. Only display geometry changes; the boundary/stop scope is fixed.
  display_file <- file.path(data_dir, "study_transit_display_paths_2025.json")
  display <- if (file.exists(display_file)) jsonlite::fromJSON(display_file,
    simplifyVector = FALSE) else NULL
  if (!is.null(display)) stopifnot(
    identical(display$snapshotDate, model$snapshotDate),
    identical(display$originalModelMd5, unname(tools::md5sum(file.path(data_dir,
      "study_transit_routes_2025.json")))),
    identical(display$boundaryMd5, unname(tools::md5sum(file.path(data_dir,
      "ftz_boundary_2025.geojson")))))
  patterns <- model$patterns
  ids <- vapply(patterns, `[[`, character(1), "id")
  stopifnot(!anyDuplicated(ids))
  stops <- project$transport$stops
  # At this stage inside_study is not yet added by app.R.
  inside <- inside_study_boundary(project, stops$longitude, stops$latitude)
  allowed <- stops$display_id[inside]
  patterns <- lapply(patterns, function(pattern) {
    stopifnot(pattern$sourceRouteId %in% project$transport$source_routes$route_uid,
      pattern$directionId %in% c("0", "1"),
      pattern$shapeId %in% names(model$shapes),
      all(unlist(pattern$stopIds) %in% stops$display_id))
    display_id <- if (!is.null(display)) display$patternShapes[[pattern$id]] else NULL
    pattern$paths <- if (!is.null(display_id)) display$paths[[display_id]] else
      model$shapes[[pattern$shapeId]]
    stopifnot(is.list(pattern$paths))
    mask_id <- if (!is.null(display)) display$patternMaskIds[[pattern$id]] else NULL
    if (!is.null(mask_id)) pattern$basemapMaskId <- mask_id
    pattern$localStopIds <- unname(as.list(unlist(pattern$stopIds)[unlist(pattern$stopIds) %in% allowed]))
    # Full external stop sequences stay in the local provenance JSON, not the browser.
    pattern$sourceStopIds <- NULL
    pattern
  })
  pattern_index <- split(seq_along(patterns), vapply(patterns, function(p)
    paste(p$sourceRouteId, p$directionId, sep="\034"), character(1)))
  project$transport$study_routes <- list(patterns = patterns, index = pattern_index,
    snapshot_date = model$snapshotDate,
    basemap_masks = if (!is.null(display)) display$basemapMasks else NULL,
    basemap_display_boundary = if (!is.null(display)) display$initialBasemapMask else NULL)
  routes <- project$transport$routes
  destinations <- vapply(seq_len(nrow(routes)), function(i) {
    options <- study_route_patterns_for_route(project$transport, routes[i, ])
    if (!length(options)) return("")
    pattern <- options[[1]]
    if (nzchar(pattern$headsign)) pattern$headsign else pattern$destination
  }, character(1))
  routes$direction_destination <- destinations
  routes$direction_title <- paste0("Direction ", routes$direction_label,
    ifelse(nzchar(destinations), paste0(" · Towards ", destinations), ""))
  routes$title <- paste(c(tram = "Tram", bus = "Bus", rail = "Rail")[routes$mode],
    routes$badge, "·", routes$direction_title)
  project$transport$routes <- routes
  project
}

study_route_patterns_for_route <- function(transport, route) {
  if (is.null(transport$study_routes)) return(list())
  aliases <- transport$route_aliases
  sources <- aliases$source_route_uid[aliases$canonical_route_uid == route$source_route_uid]
  keys <- paste(sources, route$direction_id, sep="\034")
  index <- unlist(transport$study_routes$index[keys], use.names=FALSE)
  rows <- transport$study_routes$patterns[index]
  if (!length(rows)) return(list())
  order <- order(vapply(rows, function(p) p$sourceRouteId != route$source_route_uid, logical(1)),
    -vapply(rows, function(p) as.integer(p$isDefault), integer(1)),
    -vapply(rows, function(p) length(unique(unlist(p$stopIds))), integer(1)),
    -vapply(rows, function(p) as.numeric(p$sourceStopCount), numeric(1)),
    -vapply(rows, function(p) as.numeric(p$tripsInSnapshot), numeric(1)),
    vapply(rows, `[[`, character(1), "id"))
  rows <- rows[order]
  # Outside-area differences do not require separate identical map previews.
  signatures <- vapply(rows, function(p) paste(p$shapeId, p$headsign,
    paste(unlist(p$stopIds), collapse = "|"), jsonlite::toJSON(p$stopHeadsigns,auto_unbox=TRUE), sep = "\034"), character(1))
  unname(rows[!duplicated(signatures)])
}
