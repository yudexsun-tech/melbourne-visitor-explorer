# Register the supplied route module through the existing page registry.
# extra_pages is initialized by R/extra_pages.R before this file is sourced.
source(file.path("R", "route_planner.R"), local = TRUE, encoding = "UTF-8")

extra_pages$smart_routes <- list(
  title = "Smart routes",
  ui = route_planner_ui,
  server = route_planner_server
)
