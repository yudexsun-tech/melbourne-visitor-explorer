extra_pages <- list(
  # analysis = list(
  #   ui = function(id) {
  #     ns <- NS(id)
  #   },
  #   server = function(id, context) {
  #     moduleServer(id, function(input, output, session) {
  #       output$period <- renderText(paste(context$month(), context$hour()))
  #     })
  #   }
  # )
)

extra_page_links_ui <- function() {
  if (!length(extra_pages)) return(NULL)
  div(class = "extra-page-links", lapply(names(extra_pages), function(key) {
    tags$button(type = "button", class = "panel-toggle", `data-page-target` = key,
                extra_pages[[key]]$title)
  }))
}

extra_page_tabs <- function() {
  stopifnot(!any(names(extra_pages) %in% c("map", "detail")),
            !anyDuplicated(names(extra_pages)))
  lapply(names(extra_pages), function(key) {
    page <- extra_pages[[key]]
    tabPanel(title = page$title, value = key,
      div(class = "extension-shell",
        tags$button(type = "button", class = "back-button", `data-page-target` = "map", "← Back to map"),
        page$ui(paste0("page_", key))))
  })
}

register_extra_pages <- function(context) {
  for (key in names(extra_pages)) {
    extra_pages[[key]]$server(paste0("page_", key), context)
  }
}
