# Shared attraction tooltip: preserve the teammate's main-map HTML and values.
# row is one row from get_monthly_snapshot(); weather/day_type are its filters.
# Returns character HTML. Pass through htmltools::HTML when used as a Leaflet label.
attraction_tooltip_html <- function(project, row, month, hour, weather, day_type) {
  text <- if (identical(row$opening_status, "closed")) "Closed at this hour" else
    if (identical(row$opening_status, "unknown")) "Opening hours: Not available" else
    if (is.na(row$display_count)) "Not available" else
    paste0(format_count(round_crowd_count(row$display_count)), " avg pedestrians / hour · ", row$status_zh)
  as.character(htmltools::div(class = "marker-tooltip",
    htmltools::tags$strong(paste(row$number, row$name_en)), htmltools::tags$br(),
    htmltools::tags$span(sprintf("%s · %02d:00", project$month_labels[[month]], hour)), htmltools::tags$br(),
    htmltools::tags$span(paste(if (weather == "good_weather") "Good weather" else "Bad weather",
      if (day_type == "holiday") "Holiday" else "Weekdays", sep = " · ")), htmltools::tags$br(),
    htmltools::tags$b(text)))
}
