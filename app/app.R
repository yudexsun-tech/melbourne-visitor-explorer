# Load this local app configuration even when Run App is used in an existing RStudio session.
# Please make sure you have internet connection and choose Run External before run the app
if (file.exists(".Renviron")) readRenviron(".Renviron")

required_packages <- c("shiny", "leaflet", "htmltools", "jsonlite", "later", "htmlwidgets", "mapgl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                            logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("download R package：", paste(missing_packages, collapse = ", "))
}
library(shiny)
library(mapgl)
library(leaflet)

source(file.path("R", "data_helpers.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "attraction_tooltip.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "detail_page.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "detail_data.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "extra_pages.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "app_ui.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "app_server.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "offline_layers.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "mapbox_main.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "main_map_lighting.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "weather_transport.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "full_bus_routes.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "study_transit_routes.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "monthly_profiles.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "attraction_area_crowd.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "attraction_hours.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "sensor_heat.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "bus_stop_crowd.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "transport_stop_crowd.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "route_planner_bootstrap.R"), local = TRUE, encoding = "UTF-8")
source(file.path("R", "project_cache.R"), local = TRUE, encoding = "UTF-8")
project <- load_cached_project(function() {
project <- load_map_data("data")
project <- load_offline_layers(project, "data")
project <- load_weather_transport(project, "data")
project <- load_full_bus_routes(project, "data")
project <- load_study_transit_routes(project, "data")
project <- load_monthly_profiles(project, "data")
project <- load_sensor_heat(project, "data")
project <- load_pooled_attraction_profiles(project, "data")
project <- load_bus_stop_crowd(project, "data")
project <- load_transport_stop_crowd(project, "data")
project <- load_attraction_detail_data(project, "data")
project <- load_attraction_opening_hours(project, "data")
project <- load_attraction_area_crowd(project, "data")
project$transport$stops$inside_study <- with(project$transport$stops,
  inside_study_boundary(project, longitude, latitude))
  project
})
initial_month <- if ("2025-05" %in% project$months) "2025-05" else project$months[1]
asset_url <- function(path) paste0(path, "?v=", unname(tools::md5sum(file.path("www", path))))

ui <- build_app_ui(project, initial_month, asset_url)
server <- build_app_server(project, initial_month)
shinyApp(ui = ui, server = server)
