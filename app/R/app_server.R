build_app_server <- function(project, initial_month) {
function(input, output, session) {
  player <- reactiveValues(hour = 0L, status = "ready")
  selected_month <- reactiveVal(initial_month)
  selected_weather <- reactiveVal("good_weather")
  selected_day_type <- reactiveVal("weekdays")
  selected_profile <- reactive(project$monthly_profiles[[profile_key(
    selected_month(), selected_weather(), selected_day_type())]])
  selected_attraction <- reactiveVal(NULL)
  generation <- 0L
  session_closed <- FALSE
  observe({
    req(input$offline_map_ready)
    session$sendCustomMessage("heatmap-frame", get_sensor_heat_frame(project,
      selected_month(), selected_weather(), selected_day_type(), player$hour,
      enabled = isTRUE(input$show_sensor_heat)))
  })


  # Only the main map owns precipitation. Wait for its actual style to load so
  # rapid filter/navigation changes before first paint use the latest state.
  observe({
    req(input$main_map_style_ready)
    update_main_map_rain(mapgl::mapboxgl_proxy("map", session = session),
      selected_weather(), active = identical(input$page, "map"))
  })

  last_light_key <- NULL
  observe({
    req(input$main_map_style_ready, identical(input$page, "map"))
    lighting <- main_map_light_frame(selected_month(), player$hour)
    key <- paste(input$main_map_style_ready, lighting$preset)
    if (!identical(key, last_light_key)) {
      mapgl::set_config_property(mapgl::mapboxgl_proxy("map", session=session),
        import_id="basemap", config_name="lightPreset", value=lighting$preset)
      last_light_key <<- key
    }
    session$sendCustomMessage("main-map-lighting", lighting)
  })

  set_hour <- function(hour, synchronize_slider = TRUE) {
    player$hour <- as.integer(hour)
    if (synchronize_slider) {
      session$sendCustomMessage("set-map-hour", list(hour = as.integer(hour)))
    }
  }
  stop_playback <- function(status = "paused") {
    generation <<- generation + 1L
    player$status <- status
  }

  schedule_step <- function(token) {
    delay <- isolate(suppressWarnings(as.numeric(input$speed)))
    if (length(delay) != 1 || is.na(delay) || !delay %in% c(0.5, 1.2, 2)) delay <- 1.2
    later::later(function() {
      if (session_closed || token != generation) return(invisible(NULL))
      withReactiveDomain(session, isolate({
        if (player$status != "playing") return(invisible(NULL))
        if (player$hour >= 23L) {
          stop_playback("complete")
        } else {
          set_hour(player$hour + 1L)
          schedule_step(token)
        }
      }))
    }, delay = delay)
  }
  begin_playback <- function(from_hour) {
    generation <<- generation + 1L
    set_hour(from_hour)
    player$status <- "playing"
    schedule_step(generation)
  }
  session$onSessionEnded(function() {
    session_closed <<- TRUE
    generation <<- generation + 1L
  })

  pending_filter_echo <- reactiveVal(NULL)
  apply_shared_filters <- function(month, weather, day, synchronize = FALSE) {
    if (length(month)!=1L || !month %in% project$months || length(weather)!=1L ||
        !weather %in% c("good_weather","bad_weather") || length(day)!=1L || !day %in% c("holiday","weekdays")) return()
    stop_playback("ready"); selected_month(month); selected_weather(weather); selected_day_type(day); set_hour(0L)
    if (synchronize) {
      pending_filter_echo(c(month,weather,day))
      updateSelectInput(session,"selected_month",selected=month)
      updateRadioButtons(session,"weather_filter",selected=weather)
      updateRadioButtons(session,"day_type",selected=day)
    }
  }
  observeEvent(list(input$selected_month, input$weather_filter, input$day_type), {
    req(input$selected_month,input$weather_filter,input$day_type)
    incoming <- c(input$selected_month,input$weather_filter,input$day_type)
    pending <- pending_filter_echo()
    if (!is.null(pending)) {
      if (identical(incoming,pending)) pending_filter_echo(NULL)
      return()
    }
    if (!identical(incoming,c(selected_month(),selected_weather(),selected_day_type())))
      apply_shared_filters(input$selected_month,input$weather_filter,input$day_type)
  })
  observeEvent(input$map_filter_intent, {
    field<-input$map_filter_intent$field;value<-input$map_filter_intent$value
    if(length(field)!=1L||length(value)!=1L||is.na(field)||is.na(value))return()
    month<-selected_month();weather<-selected_weather();day<-selected_day_type()
    if(field=="month")month<-value else if(field=="weather")weather<-value else if(field=="day_type")day<-value else return()
    apply_shared_filters(month,weather,day,TRUE)
    # Acknowledge only the main Mapbox UI queue; legacy callers keep their contract.
    if (length(input$map_filter_intent$mainMapSequence) == 1L)
      session$sendCustomMessage("main-map-filter-ack", list(sequence=input$map_filter_intent$mainMapSequence))
  })
  observeEvent(input$manual_hour, {
    hour <- suppressWarnings(as.numeric(input$manual_hour))
    if (length(hour) != 1 || is.na(hour) || !hour %in% 0:23) return()
    stop_playback("paused")
    set_hour(hour, synchronize_slider = FALSE)
  })
  observeEvent(input$play_day, {
    if (player$status == "playing") stop_playback("paused")
    else begin_playback(if (player$hour >= 23L) 0L else player$hour)
  })
  observeEvent(input$speed, {
    if (player$status == "playing") {
      generation <<- generation + 1L
      schedule_step(generation)
    }
  }, ignoreInit = TRUE)

  snapshot <- reactive(get_monthly_snapshot(project, selected_month(), selected_weather(),
                                           selected_day_type(), player$hour))
  output$weather_banner <- renderUI(monthly_banner_ui(project, selected_month(), selected_weather(),
                                                     selected_day_type(), player$hour))
  nearby_stops <- reactive({
    radius <- suppressWarnings(as.numeric(input$nearby_radius))
    if (length(radius) != 1L || is.na(radius) || !radius %in% c(300, 500, 1000)) radius <- 300
    get_nearby_transport(project, selected_attraction(), radius)
  })
  scoped_stops <- reactive({
    if (isTRUE(input$show_all_transport)) project$transport$stops else nearby_stops()
  })
  output$selected_context <- renderUI({
    id <- selected_attraction()
    if (is.null(id)) return(div(class = "selected-context",
      tags$h2("Select an attraction")))
    row <- snapshot()
    row <- row[row$attraction_id == id, , drop = FALSE]
    div(class = "selected-context", `data-selected-attraction` = id,
      tags$h2(row$name_en),
      div(class = "selected-summary",
        tags$span(tags$i(class = paste("legend-dot", row$color_key)),
          if (is.na(row$display_count)) detail_missing else detail_value(row$status_zh)),
        tags$span(sprintf("%02d:00 · %s · %s · %s", player$hour,
          project$month_labels[[selected_month()]],
          if (selected_weather() == "good_weather") "Good weather" else "Bad weather",
          if (selected_day_type() == "holiday") "Holiday" else "Weekdays"))),
      div(class = "selected-actions",
        actionButton("open_selected_detail", "View details", class = "transport-reset"),
        actionButton("clear_selected_attraction", "Clear selection", class = "transport-reset")))
  })
  output$nearby_status <- renderText({
    stops <- scoped_stops()
    visible <- sum(stops$inside_study & stops$mode %in% input$transport_modes)
    if (isTRUE(input$show_all_transport)) return(sprintf("%d stops", visible))
    if (is.null(selected_attraction())) return("")
    sprintf("%d nearby stops", visible)
  })
  outputOptions(output, "nearby_status", suspendWhenHidden = FALSE)
  preferred_direction <- reactiveVal("")
  pending_transport_line <- reactiveVal(NULL)
  available_transport_routes <- reactive({
    stops <- scoped_stops()
    ids <- unique(project$transport$links$route_uid[project$transport$links$display_id %in% stops$display_id])
    # A badge can be reached through the extra stops of the highlighted route.
    # Keep that selected line available even outside the nearby-stop subset.
    ids <- unique(c(ids, preferred_direction()))
    project$transport$routes[project$transport$routes$route_uid %in% ids, , drop = FALSE]
  })
  active_transport_route <- reactive({
    if (identical(pending_transport_line(), "")) return("")
    preferred <- preferred_direction()
    if (preferred %in% project$transport$routes$route_uid) return(preferred)
    line <- input$transport_route
    if (is.null(line) || !nzchar(line)) return("")
    routes <- available_transport_routes()
    choices <- routes$route_uid[routes$source_route_uid == line]
    if (!length(choices)) return("")
    if (!is.null(input$transport_direction) && input$transport_direction %in% choices)
      return(input$transport_direction)
    choices[1]
  })
  observe({
    routes <- available_transport_routes()
    lines <- routes[!duplicated(routes$source_route_uid), , drop = FALSE]
    current <- isolate(input$transport_route)
    preferred <- match(preferred_direction(), routes$route_uid)
    if (!is.na(preferred)) current <- routes$source_route_uid[preferred]
    pending <- pending_transport_line()
    if (!is.null(pending)) current <- pending
    if (is.null(current) || !current %in% lines$source_route_uid) current <- ""
    updateSelectInput(session, "transport_route", choices = c("No selected route" = "",
      stats::setNames(lines$source_route_uid, lines$line_title)), selected = current)
  })
  output$transport_direction_ui <- renderUI({
    routes <- available_transport_routes()
    line <- input$transport_route
    preferred <- preferred_direction()
    if (preferred %in% routes$route_uid)
      line <- routes$source_route_uid[match(preferred, routes$route_uid)]
    route <- isolate(active_transport_route())
    if (!nzchar(route)) return(NULL)
    routes <- routes[routes$source_route_uid == line, , drop = FALSE]
    selectInput("transport_direction", "Direction",
      choices = stats::setNames(routes$route_uid, routes$direction_title),
      selected = route, selectize = FALSE)
  })
  observeEvent(input$transport_direction, {
    if (!is.null(pending_transport_line())) return()
    if (identical(input$transport_direction, isolate(active_transport_route()))) return()
    if (input$transport_direction %in% available_transport_routes()$route_uid[
        available_transport_routes()$source_route_uid == input$transport_route])
      preferred_direction("")
  })
  observeEvent(input$reset_transport, {
    # Keep the cleared value while route choices are refreshed; otherwise the
    # old browser value can be sent back after this update and restore the pin.
    pending_transport_line(if (!identical(input$transport_route, "")) "" else NULL)
    preferred_direction("")
    updateSelectInput(session, "transport_route", selected = "")
  })
  observeEvent(input$transport_route, {
    pending <- pending_transport_line()
    if (!is.null(pending)) {
      if (!identical(input$transport_route, pending)) return()
      pending_transport_line(NULL)
    }
    if (!preferred_direction() %in% project$transport$routes$route_uid[
        project$transport$routes$source_route_uid == input$transport_route])
      preferred_direction("")
    position <- match(input$transport_route, project$transport$routes$source_route_uid)
    if (!is.na(position)) {
      mode <- project$transport$routes$mode[position]
      if (!mode %in% input$transport_modes) {
        updateCheckboxGroupInput(session, "transport_modes", selected = unique(c(input$transport_modes, mode)))
      }
    }
  })
  observeEvent(input$route_from_badge, {
    index <- match(input$route_from_badge, project$transport$routes$route_uid)
    if (length(index) != 1L || is.na(index)) return()
    line <- project$transport$routes$source_route_uid[index]
    pending_transport_line(if (!identical(input$transport_route, line)) line else NULL)
    preferred_direction(input$route_from_badge)
    updateSelectInput(session, "transport_route",
      selected = line)
    updateTabsetPanel(session, "explore_tab", selected = "transport")
  })
  observe({
    req(input$offline_map_ready)
    route <- active_transport_route()
    session$sendCustomMessage("transport-settings", list(
      modes = unname(as.list(input$transport_modes)), route = route,
      stopIds = unname(as.list(scoped_stops()$display_id))))
  })
  output$clock <- renderUI({
    div(class = "clock-row", clock_svg(player$hour),
        div(class = "clock-text", tags$time(class = "digital-clock", sprintf("%02d:00", player$hour)),
            tags$span(class = "clock-date", project$month_labels[[selected_month()]])))
  })
  output$hour_caption <- renderText(sprintf("%02d:00 – %02d:59", player$hour, player$hour))
  output$playback_status <- renderText({
    switch(player$status, ready = "",
           playing = sprintf("Playing · %d / 24", player$hour + 1L),
           paused = "Paused", complete = "Playback complete")
  })
  outputOptions(output, "playback_status", suspendWhenHidden = FALSE)
  observe({
    updateActionButton(session, "play_day",
                       label = if (player$status == "playing") "Ⅱ Pause" else "▶ Play")
  })
  output$attraction_list <- renderUI({
    rows <- isolate(snapshot())
    tagList(lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      tags$button(type = "button", class = "attraction-card",
        `data-attraction-id` = row$attraction_id,
        `aria-label` = paste("Select", row$name_en, "to view nearby stops"),
        tags$img(class = "list-marker", src = row$icon_url, alt = paste("Attraction", row$number)),
        div(class = "attraction-copy", tags$strong(row$name_en),
            div(class = "attraction-status",
                if (!is.na(row$display_count)) tags$i(class = paste("legend-dot", row$color_key)),
                tags$span(row$status_zh),
                tags$span(class = "count-value", if (is.na(row$display_count)) "Not available" else
                            paste0(format_count(round_crowd_count(row$display_count)), " avg / hour")))),
        tags$span(class = "card-arrow", "›"))
    }))
  })

  output$map <- mapgl::renderMapboxgl({
    make_mapbox_main(project, initial_month)
  })
  observe({
    req(input$offline_map_ready)
    rows <- snapshot()
    points <- unname(lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      list(id = row$attraction_id, icon = row$icon_url, color = row$color_key,
        status = row$status_zh, count = if (is.na(row$display_count)) "Not available" else
          paste0(format_count(round_crowd_count(row$display_count)), " avg / hour"),
        hasData = !is.na(row$display_count),
        tooltip = attraction_tooltip_html(project, row, selected_month(), player$hour,
          selected_weather(), selected_day_type()))
    }))
    session$sendCustomMessage("attraction-frame", list(points = points))
  })

  select_attraction <- function(id) {
    if (length(id) != 1 || is.na(id) || !id %in% project$attractions$attraction_id) return()
    selected_attraction(id)
    updateCheckboxInput(session, "show_all_transport", value = FALSE)
    updateSelectInput(session, "transport_route", selected = "")
    updateTabsetPanel(session, "explore_tab", selected = "transport")
  }
  # Keep the teammate's map/list selection workflow: reveal nearby Transport.
  # The explicit View details action below is the separate detail-page entrance.
  observeEvent(input$map_marker_click, { select_attraction(input$map_marker_click$id) })
  observeEvent(input$selected_from_list, { select_attraction(input$selected_from_list) })
  observeEvent(input$open_selected_detail, {
    req(selected_attraction())
    if (player$status == "playing") stop_playback("paused")
    updateTabsetPanel(session, "page", selected = "detail")
  })
  observeEvent(input$clear_selected_attraction, {
    selected_attraction(NULL)
    updateCheckboxInput(session, "show_all_transport", value = FALSE)
    updateSelectInput(session, "transport_route", selected = "")
  })
  observe({
    req(input$offline_map_ready)
    session$sendCustomMessage("selected-attraction", list(id = selected_attraction()))
  })
  observeEvent(input$back_to_map, {
    updateTabsetPanel(session, "page", selected = "map")
    session$sendCustomMessage("resize-city-map", list())
  })
  # Reviews follow only the selected attraction, never crowd/weather/month filters.
  observe({
    req(input$reviews_ready)
    id <- selected_attraction()
    if (is.null(id) || !id %in% project$attractions$attraction_id) id <- ""
    session$sendCustomMessage("review-attraction", list(
      attractionId = id, active = identical(input$page, "detail")))
  })
  output$detail_page <- renderUI({
    req(selected_attraction())
    id <- selected_attraction()
    row <- project$attractions[project$attractions$attraction_id == id, , drop = FALSE]
    tagList(
      div(class = "detail-toolbar", actionButton("back_to_map", "← Back to map", class = "back-button"),
          tags$span("Attraction details")),
      div(class = "detail-header", tags$span(class = "detail-number", row$number),
          div(class="detail-heading-copy",tags$h1(row$name_en),detail_setting_badge(project,id))),
      attraction_detail_ui(id, row, NS("attraction_detail"), project = project,
        settings = isolate(list(month = selected_month(), weather = selected_weather(),
          day_type = selected_day_type(), hour = player$hour)))
    )
  })

  context <- list(month = reactive(selected_month()), weather = reactive(selected_weather()),
                  day_type = reactive(selected_day_type()), dates = reactive(selected_profile()$dates),
                  hour = reactive(player$hour),
                  attraction_id = reactive(selected_attraction()), project = project,
                  page = reactive(input$page),
                  show_transport = function() {
                    id <- selected_attraction()
                    if (length(id) != 1L || is.na(id) || !id %in% project$attractions$attraction_id)
                      return(invisible(FALSE))
                    select_attraction(id)
                    updateTabsetPanel(session, "page", selected = "map")
                    session$sendCustomMessage("resize-city-map", list())
                    invisible(TRUE)
                  },
                  set_hour = function(hour) {
                    if (length(hour) == 1L && !is.na(hour) && hour %in% 0:23) {
                      stop_playback("paused"); set_hour(hour)
                    }
                  },
                  set_filters = function(month, weather, day_type) {
                    apply_shared_filters(month,weather,day_type,TRUE)
                  })
  attraction_detail_server("attraction_detail", context)
  register_extra_pages(context)
  observeEvent(input$navigate_page, {
    target <- input$navigate_page
    if (length(target) != 1L || !target %in% c("map", names(extra_pages))) return()
    if (player$status == "playing") stop_playback("paused")
    updateTabsetPanel(session, "page", selected = target)
    if (target == "map") session$sendCustomMessage("resize-city-map", list())
  })
}
}
