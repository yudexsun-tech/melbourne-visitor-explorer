# Resolve and read the main-map initializer while this module is sourced.
# Reactive rendering can run after RStudio/Shiny restores another working
# directory; it must not reopen a relative www/ path at that later point.
.mapbox_main_assets <- local({
  source_files <- vapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) == 1L) path else ""
  }, character(1))
  module_file <- tail(source_files[basename(source_files) == "mapbox_main.R"], 1L)
  script_file <- if (length(module_file)) {
    file.path(dirname(module_file), "..", "www", "map_init.js")
  } else {
    file.path("www", "map_init.js")
  }
  script_file <- normalizePath(script_file, winslash = "/", mustWork = FALSE)
  if (!file.exists(script_file)) {
    stop("Main-map script is missing: ", script_file,
         ". Fully extract the project ZIP and restart the app.", call. = FALSE)
  }
  list(
    initializer = paste(readLines(script_file, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
    transportIconVersion = paste(unname(tools::md5sum(file.path(dirname(script_file),
      "transport", paste0(c("bus", "tram", "rail"), ".svg")))), collapse = "")
  )
})

# Main map only. Detail and Smart routes retain their original Leaflet builders.
make_mapbox_main <- function(project, initial_month = if("2025-05" %in% project$months) "2025-05" else project$months[1]) {
  token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN", unset = Sys.getenv("MAPBOX_ACCESS_TOKEN"))
  shiny::validate(shiny::need(startsWith(token, "pk."),
    "Set MAPBOX_PUBLIC_TOKEN to your public Mapbox token, then restart the app."))
  ring <- project$boundary$features[[1]]$geometry$coordinates[[1]]
  coordinates <- do.call(rbind, lapply(ring, unlist))
  box <- list(c(min(coordinates[,1]), min(coordinates[,2])),
              c(max(coordinates[,1]), max(coordinates[,2])))
  # GL's 512px world uses one zoom level less than Leaflet's 256px world.
  map <- mapgl::mapboxgl(
    style = "mapbox://styles/yuuki2004526/cmvxdpjg7001h01st5hlb309n",
    access_token = token, projection = "mercator", center = c(144.956, -37.813),
    config = list(basemap = list(lightPreset = main_map_light_frame(initial_month, 0)$preset)),
    zoom = 13, minZoom = 12.5, maxZoom = 18, maxBounds = box,
    pitch = 0, bearing = 0, maxPitch = 60, dragRotate = TRUE,
    touchPitch = TRUE, renderWorldCopies = FALSE, attributionControl = FALSE)
  initializer <- .mapbox_main_assets$initializer
  htmlwidgets::onRender(map, paste0(
    "function(el,x,data) { var surface = window.createMapboxMainSurface(el,this.getMap(),data); (",
    initializer, ").call(surface,el,x,data); surface.addAttractions(data.attractions); }"),
    data = list(transport = transport_browser_data(project, icon_version = .mapbox_main_assets$transportIconVersion), studyBoundary = ring,
      streetHeatMask = project$street_heat_mask,
      attractions = unname(lapply(seq_len(nrow(project$attractions)), function(i) {
        row <- project$attractions[i,]
        list(id=row$attraction_id, lng=row$longitude, lat=row$latitude, name=row$name_en, icon=paste0("icons/", row$icon_key, "-unknown.svg"))
      }))))
}

# A visible weather cue only: no change to crowd data, traffic or weather classes.
update_main_map_rain <- function(map, weather, active = TRUE) {
  if (!isTRUE(active) || !identical(weather, "bad_weather")) {
    return(mapgl::set_rain(map, remove = TRUE))
  }
  mapgl::set_rain(map, density = 0.38, intensity = 0.80,
    color = "#b3c6d8", opacity = 0.80, center_thinning = 0,
    direction = c(0, 80), droplet_size = c(2.4, 20),
    distortion_strength = 0, vignette = 0)
}
