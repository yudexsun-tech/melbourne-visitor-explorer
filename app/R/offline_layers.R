load_offline_layers <- function(project, data_dir) {
  project$offline <- jsonlite::fromJSON(file.path(data_dir, "offline_config.json"))
  project$basemap <- lapply(c("roads", "water", "parks"), function(group) {
    paste(readLines(file.path(data_dir, "basemap", paste0(if (group == "roads") "roads_display" else group, ".geojson")),
                    encoding = "UTF-8", warn = FALSE), collapse = "")
  })
  names(project$basemap) <- c("roads", "water", "parks")
  project$street_heat_mask <- jsonlite::fromJSON(
    file.path(data_dir, "street_heat", "mask_display.geojson"), simplifyVector = FALSE)
  project$street_labels <- read.csv(file.path(data_dir, "basemap", "street_labels.csv"),
                                   stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  project
}

make_offline_basemap <- function(project, initialize = TRUE) {
  ring <- project$boundary$features[[1]]$geometry$coordinates[[1]]
  coordinates <- do.call(rbind, lapply(ring, unlist))
  b <- list(west = min(coordinates[,1]), east = max(coordinates[,1]),
            south = min(coordinates[,2]), north = max(coordinates[,2]))
  map <- leaflet(options = leafletOptions(attributionControl = FALSE,
    minZoom = 13.5, maxZoom = 19, zoomSnap = .25, zoomDelta = .5,
    preferCanvas = TRUE, maxBoundsViscosity = 1, zoomAnimation = FALSE, markerZoomAnimation = FALSE
  ))
  map <- addGeoJSON(map, project$basemap$parks, group = "offline_parks",
                    color = "#d7e3ce", weight = .5, opacity = 1,
                    fillColor = "#e2ecd8", fillOpacity = 1,
                    options = pathOptions(interactive = FALSE))
  map <- addGeoJSON(map, project$basemap$water, group = "offline_water",
                    color = "#c7dde2", weight = .6, opacity = 1,
                    fillColor = "#d8e8ed", fillOpacity = 1,
                    options = pathOptions(interactive = FALSE))
  map <- addGeoJSON(map, project$basemap$roads, group = "offline_roads",
                    color = "#c9d0c2", weight = 2.3, opacity = .8, fill = FALSE,
                    options = pathOptions(interactive = FALSE))
  map <- addGeoJSON(map, project$basemap$roads, group = "offline_road_centres",
                    color = "#ffffff", weight = 1.15, opacity = 1, fill = FALSE,
                    options = pathOptions(interactive = FALSE))
  labels <- project$street_labels
  label_html <- lapply(seq_len(nrow(labels)), function(i) htmltools::HTML(sprintf(
    '<span class="street-label" style="transform:rotate(%sdeg)">%s</span>',
    labels$angle[i], htmltools::htmlEscape(labels$name[i])
  )))
  map <- addLabelOnlyMarkers(map, lng = labels$longitude, lat = labels$latitude,
    label = label_html, group = "offline_street_names",
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                className = "offline-street-label", opacity = .9),
    options = markerOptions(interactive = FALSE, keyboard = FALSE))
  map <- addControl(map, html = paste0('&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    ' · <a href="https://data.melbourne.vic.gov.au/" title="Street mask: City of Melbourne, CC BY 4.0">City of Melbourne</a>'),
                    position = "bottomright", className = "offline-attribution")
  map <- setMaxBounds(map, b$west, b$south, b$east, b$north)
  if (!isTRUE(initialize)) return(map)
  htmlwidgets::onRender(map,
    paste(readLines("www/map_init.js", encoding = "UTF-8", warn = FALSE), collapse = "\n"),
    data = list(transport = transport_browser_data(project),
                studyBoundary = ring, streetHeatMask = project$street_heat_mask))
}

inside_study_boundary <- function(project, longitude, latitude) {
  ring <- do.call(rbind, lapply(project$boundary$features[[1]]$geometry$coordinates[[1]], unlist))
  inside <- rep(FALSE, length(longitude)); on_edge <- inside
  for (i in seq_len(nrow(ring) - 1L)) {
    a <- ring[i, ]; b <- ring[i + 1L, ]
    cross <- (longitude-a[1])*(b[2]-a[2]) - (latitude-a[2])*(b[1]-a[1])
    on_edge <- on_edge | (abs(cross) < 1e-12 &
      longitude >= min(a[1],b[1])-1e-10 & longitude <= max(a[1],b[1])+1e-10 &
      latitude >= min(a[2],b[2])-1e-10 & latitude <= max(a[2],b[2])+1e-10)
    if (a[2] == b[2]) next
    crossing <- (a[2] > latitude) != (b[2] > latitude)
    x <- (b[1]-a[1])*(latitude-a[2])/(b[2]-a[2])+a[1]
    inside <- xor(inside, crossing & longitude < x)
  }
  inside | on_edge
}
