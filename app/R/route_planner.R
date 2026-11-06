# Keep browser URLs relative to the app, but resolve filesystem reads against
# this module's app directory rather than the later reactive working directory.
.route_planner_root <- local({
  source_files <- vapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) == 1L) path else ""
  }, character(1))
  module_file <- tail(source_files[basename(source_files) == "route_planner.R"], 1L)
  normalizePath(if (length(module_file)) file.path(dirname(module_file), "..") else ".",
                winslash = "/", mustWork = TRUE)
})

route_planner_places <- function(project) {
  a <- project$attractions
  
  places <- data.frame(
    id = as.character(a$attraction_id),
    name = as.character(a$name_en),
    lat = as.numeric(a$latitude),
    lng = as.numeric(a$longitude),
    stringsAsFactors = FALSE
  )
  
  parks <- data.frame(
    id = "PARK_DOCKLANDS",
    name = "Docklands Park",
    lat = -37.82010,
    lng = 144.94675,
    stringsAsFactors = FALSE
  )
  
  places <- rbind(places, parks)
  # The main map and Smart Routes must use the same approximate study boundary.
  # Previously proposed outer parks are no longer in the candidates.
  places <- places[
    inside_study_boundary(project, places$lng, places$lat),
    ,
    drop = FALSE
  ]
  places$measured <- places$id %in% as.character(a$attraction_id)
  places$setting <- as.character(a$setting[match(places$id, a$attraction_id)])
  places$setting[!places$measured] <- "outdoor"
  
  descriptions <- c(
    ATTR001 = "Step inside one of Melbourne's best-loved libraries. Enjoy the beautiful architecture and take a quiet break.",
    ATTR002 = "Discover Australian art at NGV Australia. Take your time and enjoy the exhibitions that interest you.",
    ATTR003 = "Explore the world of film and moving images at ACMI. It is a nice indoor stop during your city visit.",
    ATTR004 = "Learn about the people and stories that shaped Melbourne at the Immigration Museum.",
    ATTR005 = "Take a moment to admire this beautiful historic cathedral in the heart of Melbourne.",
    ATTR006 = "Enjoy the colours and lively atmosphere of Queen Victoria Market. You might like to stop for a snack.",
    ATTR007 = "Walk through this charming historic arcade and enjoy its beautiful architecture.",
    ATTR008 = "Look up and discover the elegant details of the Block Arcade. Its covered setting is useful when the weather changes.",
    ATTR009 = "Experience one of Melbourne's best-known shopping streets and enjoy the atmosphere of the city centre.",
    ATTR010 = "Explore Melbourne's Chinatown, with its restaurants, shops and colourful streets.",
    ATTR011 = "Admire the famous Flinders Street Station building, one of Melbourne's best-known landmarks.",
    ATTR012 = "Spend some time at Federation Square and enjoy Melbourne's public spaces and cultural atmosphere.",
    ATTR016 = "Discover Melbourne's colourful street art in Hosier Lane. Every corner offers something interesting to see.",
    PARK_DOCKLANDS = "Enjoy the open space near the Docklands waterfront and see a different side of Melbourne."
  )
  
  icons <- c(
    ATTR001 = "book",
    ATTR002 = "paint-brush",
    ATTR003 = "film",
    ATTR004 = "university",
    ATTR005 = "building",
    ATTR006 = "shopping-basket",
    ATTR007 = "shopping-bag",
    ATTR008 = "shopping-bag",
    ATTR009 = "shopping-bag",
    ATTR010 = "cutlery",
    ATTR011 = "train",
    ATTR012 = "star",
    ATTR016 = "camera",
    PARK_DOCKLANDS = "tree"
  )
  
  colours <- ifelse(
    grepl("^PARK_", places$id),
    "green",
    ifelse(
      places$id %in% c(
        "ATTR006", "ATTR007", "ATTR008",
        "ATTR009", "ATTR010"
      ),
      "orange",
      ifelse(
        places$id %in% c(
          "ATTR002", "ATTR003", "ATTR004"
        ),
        "purple",
        "blue"
      )
    )
  )
  
  places$description <- unname(
    descriptions[places$id]
  )
  
  places$icon <- unname(
    icons[places$id]
  )
  
  places$colour <- colours
  
  places$category <- ifelse(
    grepl("^PARK_", places$id),
    "Park and open space",
    ifelse(
      places$id %in% c(
        "ATTR002", "ATTR003", "ATTR004"
      ),
      "Museum and culture",
      ifelse(
        places$id %in% c(
          "ATTR006", "ATTR007", "ATTR008",
          "ATTR009", "ATTR010"
        ),
        "Shopping and local life",
        "Melbourne landmark"
      )
    )
  )

  short_descriptions <- c(
    ATTR001 = "Discover Melbourne's historic library and enjoy a quiet break.",
    ATTR002 = "Enjoy Australian art in the heart of Federation Square.",
    ATTR003 = "Explore film and moving-image exhibitions.",
    ATTR004 = "Discover stories of migration and Melbourne's history.",
    ATTR005 = "Admire this historic cathedral and its architecture.",
    ATTR006 = "Browse market stalls and enjoy local food.",
    ATTR007 = "Explore a charming covered heritage arcade.",
    ATTR008 = "Discover elegant architecture in this covered arcade.",
    ATTR009 = "Experience Melbourne's central shopping street.",
    ATTR010 = "Explore Chinatown's colourful streets and food culture.",
    ATTR011 = "See Melbourne's iconic station and historic clock facade.",
    ATTR012 = "Enjoy public art and the lively Federation Square plaza.",
    ATTR016 = "See colourful street art in the city's laneways.",
    PARK_DOCKLANDS = "Enjoy a green break near the Docklands waterfront."
  )
  places$short_description <- unname(short_descriptions[places$id])
  places$photo <- vapply(places$id, function(id) {
    path <- paste0("route-photos/", id, ".jpg")
    if (file.exists(file.path(.route_planner_root, "www", path))) path else
      "route-photos/placeholder.svg"
  }, character(1))
  
  places
}


route_planner_routes <- function() {
  list(
    sunny_quiet_yes = list(
      title = "Western Melbourne Discovery",
      subtitle = "A gentle journey across western Melbourne",
      ids = c(
        "ATTR006",
        "ATTR001",
        "ATTR004",
        "PARK_DOCKLANDS"
      ),
      story = paste(
        "Enjoy a slower day across western Melbourne,",
        "from the city market to cultural stops and the waterfront."
      ),
      reason = paste(
        "This route combines green spaces,",
        "culture and opportunities to pause.",
        "Nearby facilities are highlighted",
        "to help you plan a comfortable visit."
      )
    ),
    
    sunny_quiet_no = list(
      title = "City Corners and Culture",
      subtitle = "History, culture and a relaxed atmosphere",
      ids = c(
        "ATTR006",
        "ATTR001",
        "ATTR010",
        "ATTR005",
        "ATTR012"
      ),
      story = paste(
        "Explore Melbourne's historic and cultural city corners",
        "from the market and library to the riverfront."
      ),
      reason = paste(
        "This journey brings together green spaces",
        "and cultural highlights.",
        "It is designed for visitors who enjoy",
        "a relaxed sightseeing experience."
      )
    ),
    
    sunny_lively_yes = list(
      title = "Classic Melbourne Highlights",
      subtitle = "A memorable day in the heart of the city",
      ids = c(
        "ATTR006",
        "ATTR001",
        "ATTR009",
        "ATTR008",
        "ATTR012",
        "ATTR011"
      ),
      story = paste(
        "A wonderful introduction to Melbourne.",
        "Enjoy the market, discover the State Library,",
        "explore the shopping streets and finish",
        "near the famous Flinders Street Station."
      ),
      reason = paste(
        "We selected well-known attractions",
        "and central streets.",
        "Nearby facilities are highlighted",
        "to help make your day easier."
      )
    ),
    
    sunny_lively_no = list(
      title = "Across the City Adventure",
      subtitle = "A lively journey across Melbourne",
      ids = c(
        "PARK_DOCKLANDS",
        "ATTR004",
        "ATTR008",
        "ATTR009",
        "ATTR010",
        "ATTR016"
      ),
      story = paste(
        "Discover Melbourne from west to east.",
        "Start near Docklands, visit a museum,",
        "then explore the historic arcades,",
        "shopping streets, Chinatown and Hosier Lane."
      ),
      reason = paste(
        "This route covers a broad part of the city",
        "and connects different neighbourhoods.",
        "It is ideal for visitors who enjoy variety."
      )
    ),
    
    cloudy_quiet_yes = list(
      title = "Calm Indoor Discovery",
      subtitle = "Art, history and comfortable cultural stops",
      ids = c(
        "ATTR001",
        "ATTR008",
        "ATTR004",
        "ATTR003",
        "ATTR002"
      ),
      story = paste(
        "A cloudy day can still be a lovely Melbourne day.",
        "Enjoy the State Library and Block Arcade,",
        "then discover the Immigration Museum,",
        "ACMI and Australian art at NGV."
      ),
      reason = paste(
        "This plan prioritises indoor attractions",
        "and highlights nearby facilities.",
        "Some outdoor walking is still necessary."
      )
    ),
    
    cloudy_quiet_no = list(
      title = "Heritage and Hidden Arcades",
      subtitle = "A slower journey through Melbourne's history",
      ids = c(
        "ATTR001",
        "ATTR010",
        "ATTR007",
        "ATTR008",
        "ATTR004"
      ),
      story = paste(
        "Take your time exploring Melbourne's character.",
        "Start at the State Library, pass through Chinatown",
        "and visit two beautiful historic arcades.",
        "Finish at the Immigration Museum."
      ),
      reason = paste(
        "The route focuses on cultural experiences",
        "and includes covered or indoor stops.",
        "It offers an alternative to a fully outdoor day."
      )
    ),
    
    cloudy_lively_yes = list(
      title = "Melbourne Indoor Favourites",
      subtitle = "Popular sights with comfort in mind",
      ids = c(
        "ATTR001",
        "ATTR007",
        "ATTR008",
        "ATTR003",
        "ATTR002"
      ),
      story = paste(
        "Enjoy some of Melbourne's favourite indoor places.",
        "Begin with books and architecture,",
        "walk through the historic arcades,",
        "then explore film and Australian art."
      ),
      reason = paste(
        "We have chosen popular cultural attractions",
        "and highlighted facilities nearby.",
        "Remember to check venue opening hours."
      )
    ),
    
    cloudy_lively_no = list(
      title = "Central Melbourne Culture Trail",
      subtitle = "An introduction to the city's creative side",
      ids = c(
        "ATTR007",
        "ATTR008",
        "ATTR009",
        "ATTR003",
        "ATTR002"
      ),
      story = paste(
        "Discover Melbourne's central streets",
        "and creative atmosphere.",
        "Enjoy the historic shopping arcades",
        "and finish with a visit to ACMI and NGV Australia."
      ),
      reason = paste(
        "This journey combines central landmarks",
        "with indoor cultural experiences.",
        "It suits visitors who enjoy city life."
      )
    )
  )
}


route_planner_distance <- function(lat1, lng1, lat2, lng2) {
  r <- pi / 180
  x <- (lng2 - lng1) * r *
    cos((lat1 + lat2) * r / 2)
  y <- (lat2 - lat1) * r
  
  6371 * sqrt(x^2 + y^2)
}


route_planner_comfort_counts <- function(places, facilities) {
  if (is.null(facilities) || !nrow(facilities)) {
    return(rep(0, nrow(places)))
  }
  vapply(seq_len(nrow(places)), function(i) {
    km <- route_planner_distance(
      places$lat[i], places$lng[i],
      facilities$latitude, facilities$longitude
    )
    as.numeric(sum(
      km <= 0.35 &
        facilities$facility_type %in% c("toilet", "bench", "drinking_water"),
      na.rm = TRUE
    ))
  }, numeric(1))
}

# Eight named themes are retained. The selected places and their order vary
# with the pooled 2024-2025 pedestrian snapshot, start point and preferences.
# Counts describe nearby sensor passages, never actual venue occupancy or
# a measured pedestrian count for the entire walking route.
route_planner_stops <- function(
    plan, places, snapshot, facilities,
    weather = "sunny", mood = "quiet",
    comfort = "yes", start_id = "auto") {
  x <- places
  idx <- match(x$id, snapshot$attraction_id)
  x$opening_status <- as.character(snapshot$opening_status[idx])
  x$opening_status[!x$measured] <- "unknown"
  x$opening_status[is.na(x$opening_status)] <- "unknown"

  band_column <- if ("crowd_color_key" %in% names(snapshot)) {
    "crowd_color_key"
  } else {
    "color_key"
  }
  x$crowd_band <- as.character(snapshot[[band_column]][idx])
  x$crowd_band[!x$measured | is.na(x$crowd_band)] <- "unknown"
  x$sample_days <- as.numeric(snapshot$sample_days[idx])
  x$sample_days[!x$measured] <- NA_real_
  x$flow_per_m2 <- as.numeric(snapshot$flow_per_m2[idx])
  x$flow_per_m2[!x$measured] <- NA_real_
  x$crowd_label <- unname(c(
    quiet = "Quiet nearby",
    moderate = "Moderate nearby",
    crowded = "Busy nearby",
    unknown = "No crowd estimate"
  )[x$crowd_band])

  valid <- which(is.finite(x$flow_per_m2) & x$sample_days > 0)
  percentile <- rep(NA_real_, nrow(x))
  if (length(valid)) {
    percentile[valid] <- (rank(
      x$flow_per_m2[valid], ties.method = "average"
    ) - 1) / max(1, length(valid) - 1)
  }
  crowd_preference <- if (mood == "lively") {
    2 * percentile - 1
  } else {
    1 - 2 * percentile
  }
  crowd_preference[!is.finite(crowd_preference)] <- -0.9

  # Existing theme locations are a preference, not a fixed itinerary.
  x$score <- 1.6 * as.numeric(x$id %in% plan$ids) +
    3.0 * crowd_preference
  if (weather == "cloudy") {
    x$score <- x$score + ifelse(
      x$setting %in% c("indoor", "covered"), 1.2,
      ifelse(x$setting == "outdoor", -0.7, 0.3)
    )
  } else {
    x$score <- x$score + ifelse(
      x$setting %in% c("outdoor", "mixed"), 0.8, 0
    )
  }
  x$comfort_count <- route_planner_comfort_counts(x, facilities)
  if (comfort == "yes") {
    x$score <- x$score + 0.6 * log1p(x$comfort_count)
  }
  x$score[x$opening_status == "closed"] <-
    x$score[x$opening_status == "closed"] - 8
  x$score[x$opening_status == "unknown"] <-
    x$score[x$opening_status == "unknown"] - 0.4

  # Retain the western waterfront as an optional, clearly unmeasured scenic
  # stop on the two western themes; never assign it a fake crowd category.
  if ("PARK_DOCKLANDS" %in% plan$ids &&
      "PARK_DOCKLANDS" %in% x$id) {
    x$score[x$id == "PARK_DOCKLANDS"] <-
      x$score[x$id == "PARK_DOCKLANDS"] + 2.0
  }

  wanted <- min(nrow(x), max(4L, min(6L, length(plan$ids))))
  start <- match(start_id, x$id)
  if (is.na(start)) {
    preferred <- which(x$id %in% plan$ids & x$opening_status != "closed")
    if (!length(preferred)) preferred <- seq_len(nrow(x))
    start <- preferred[which.max(x$score[preferred])]
  }
  chosen <- start
  while (length(chosen) < wanted) {
    remaining <- setdiff(seq_len(nrow(x)), chosen)
    available <- remaining[x$opening_status[remaining] != "closed"]
    if (length(available)) remaining <- available
    last <- tail(chosen, 1)
    distance <- route_planner_distance(
      x$lat[last], x$lng[last], x$lat[remaining], x$lng[remaining]
    )
    utility <- x$score[remaining] - 1.25 * distance
    chosen <- c(chosen, remaining[which.max(utility)])
  }
  result <- x[chosen, , drop = FALSE]
  result$number <- seq_len(nrow(result))
  rownames(result) <- NULL
  result
}


route_planner_facilities <- function(
    file = file.path(.route_planner_root, "data", "facilities_2025.csv")) {
  
  if (!file.exists(file)) {
    return(NULL)
  }
  
  d <- utils::read.csv(
    file,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  
  needed <- c(
    "facility_type",
    "latitude",
    "longitude",
    "wheelchair_raw"
  )
  
  if (!all(needed %in% names(d))) {
    return(NULL)
  }
  
  d$latitude <- suppressWarnings(
    as.numeric(d$latitude)
  )
  
  d$longitude <- suppressWarnings(
    as.numeric(d$longitude)
  )
  
  d <- d[
    is.finite(d$latitude) &
      is.finite(d$longitude),
    ,
    drop = FALSE
  ]
  
  d$facility_type <- tolower(
    trimws(d$facility_type)
  )
  
  d$wheelchair_raw <- tolower(
    trimws(d$wheelchair_raw)
  )
  
  d
}


route_planner_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$link(
      rel = "stylesheet",
      href = "route_planner.css"
    ),
    
    shiny::div(
      class = "rp-shell",
      
      shiny::div(
        class = "rp-intro",
        
        shiny::div(
          class = "rp-eyebrow",
          "DISCOVER MELBOURNE"
        ),
        
        shiny::tags$h1(
          "Explore Melbourne your way"
        ),
        
        shiny::tags$p(
          paste(
            "Choose what matters to you",
            "and enjoy a memorable day",
            "in Melbourne at your own pace."
          )
        )
      ),
      
      shiny::div(
        class = "rp-layout",
        
        shiny::div(
          class = "rp-sidebar",
          
          shiny::div(
            class = "rp-panel rp-preferences",
            
            shiny::tags$h2("Make it yours"),
            
            shiny::tags$p(
              class = "rp-help",
              "Tell us what kind of day you would enjoy."
            ),
            
            shiny::div(
              class = "rp-field",
              
              shiny::tags$label(
                "Weather for your visit"
              ),
              
              shiny::radioButtons(
                ns("weather"),
                NULL,
                choices = c(
                  "☀ Good walking conditions" = "sunny",
                  "☁ Poor walking conditions" = "cloudy"
                ),
                selected = "sunny",
                inline = TRUE
              )
            ),
            
            shiny::div(
              class = "rp-field",
              
              shiny::tags$label(
                "Preferred atmosphere"
              ),
              
              shiny::radioButtons(
                ns("crowd"),
                NULL,
                choices = c(
                  "Quiet and relaxed" = "quiet",
                  "Lively and busy" = "lively"
                ),
                selected = "quiet",
                inline = TRUE
              )
            ),
            
            shiny::div(
              class = "rp-field",
              
              shiny::tags$label(
                "Comfort facilities"
              ),
              
              shiny::radioButtons(
                ns("comfort"),
                NULL,
                choices = c(
                  "Important to me" = "yes",
                  "No preference" = "no"
                ),
                selected = "yes",
                inline = TRUE
              )
            ),

            shiny::div(
              class = "rp-field",
              shiny::uiOutput(ns("start_picker"))
            ),

            shiny::div(
              class = "rp-field",
              shiny::tags$label("Planned start hour"),
              shiny::selectInput(
                ns("visit_hour"), NULL,
                choices = stats::setNames(as.character(8:20), sprintf("%02d:00", 8:20)),
                selected = "11", width = "100%"
              )
            ),

            shiny::div(
              class = "rp-data-context",
              shiny::uiOutput(ns("data_context"))
            )
          ),
          
          shiny::div(
            class = "rp-panel rp-result",
            
            shiny::tags$h2(
              "Your recommended journey"
            ),
            
            shiny::uiOutput(
              ns("route_card")
            ),
            
            shiny::actionButton(
              ns("tour_btn"),
              "▶ Play guided tour",
              class = "rp-tour-button"
            )
          )
        ),
        
        shiny::div(
          class = "rp-map-column",
          
          shiny::div(
            class = "rp-map-toolbar",
            
            shiny::div(
              shiny::tags$strong(
                "Your Melbourne journey"
              ),
              
              shiny::tags$span(
                "Inside the shared study boundary"
              )
            ),
            
            shiny::div(
              class = "rp-map-tools",
              
              shiny::checkboxInput(
                ns("show_toilets"),
                "Public toilets",
                value = TRUE
              ),
              
              shiny::checkboxInput(
                ns("show_access"),
                "Wheelchair-tagged facilities",
                value = FALSE
              )
            )
          ),
          
          shiny::uiOutput(
            ns("tour_status")
          ),
          
          shiny::div(
            class = "rp-tour-controls",
            
            shiny::actionButton(
              ns("prev_btn"),
              "← Previous stop",
              class = "rp-step-button"
            ),
            
            shiny::actionButton(
              ns("next_btn"),
              "Next stop →",
              class = "rp-step-button"
            )
          ),
          
          leaflet::leafletOutput(
            ns("route_map"),
            width = "100%",
            height = "560px"
          ),
          
          shiny::div(
            class = "rp-map-legend",
            
            shiny::tags$span(
              class = "rp-legend-dot rp-blue"
            ),
            "Landmarks",
            
            shiny::tags$span(
              class = "rp-legend-dot rp-purple"
            ),
            "Museums",
            
            shiny::tags$span(
              class = "rp-legend-dot rp-orange"
            ),
            "Shopping",
            
            shiny::tags$span(
              class = "rp-legend-dot rp-green"
            ),
            "Parks",
            
            shiny::tags$span(
              class = "rp-legend-line"
            ),
            "Suggested journey",
            shiny::tags$span(
              class = "rp-legend-note",
              "Dashed connections are illustrative, not verified walking paths."
            )
          ),
          
          shiny::div(
            class = "rp-panel rp-stops-panel",
            
            shiny::tags$h2(
              "Places along your journey"
            ),
            
            shiny::tags$p(
              class = "rp-help",
              "Tap a photo card to find that place on the map."
            ),
            
            shiny::uiOutput(
              ns("stop_list"),
              class = "rp-horizontal-stops"
            ),
            shiny::tags$p(
              class = "rp-photo-credit-link",
              shiny::tags$a(
                "Photo credits and licences",
                href = "route-photos/CREDITS.html",
                target = "_blank",
                rel = "noopener noreferrer"
              )
            )
          )
        )
      )
    )
  )
}


route_planner_server <- function(id, context) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      project <- context$project
      places <- route_planner_places(project)
      plans <- route_planner_routes()
      facilities <- route_planner_facilities()

      output$start_picker <- shiny::renderUI({
        choices <- c(
          "Choose the start for me" = "auto",
          stats::setNames(places$id, places$name)
        )
        shiny::tagList(
          shiny::tags$label(`for` = session$ns("start_at"), "Your starting point"),
          shiny::selectInput(
            session$ns("start_at"), NULL,
            choices = choices, selected = "auto", width = "100%"
          )
        )
      })

      output$data_context <- shiny::renderUI({
        month <- context$month()
        day_type <- context$day_type()
        hour <- suppressWarnings(as.integer(input$visit_hour))
        shiny::div(
          shiny::tags$strong("Historical crowd context"),
          shiny::tags$p(
            paste(
              project$month_labels[[month]],
              if (day_type == "holiday") "Holidays / weekends" else "Weekdays",
              if (length(hour) == 1L && is.finite(hour)) {
                sprintf("%02d:00", hour)
              } else {
                ""
              },
              sep = " · "
            )
          ),
          shiny::tags$small(
            "Based on 2024–2025 averages. Month and day type follow the main map."
          )
        )
      })
      
      selected_stop <- shiny::reactiveVal(1L)
      touring <- shiny::reactiveVal(FALSE)
      
      plan <- shiny::reactive({
        shiny::req(
          input$weather,
          input$crowd,
          input$comfort
        )
        
        key <- paste(
          input$weather,
          input$crowd,
          input$comfort,
          sep = "_"
        )
        
        shiny::req(!is.null(plans[[key]]))
        plans[[key]]
      })

      crowd_snapshot <- shiny::reactive({
        shiny::req(input$weather, input$visit_hour)
        weather_class <- if (input$weather == "sunny") {
          "good_weather"
        } else {
          "bad_weather"
        }
        get_monthly_snapshot(
          project, context$month(), weather_class,
          context$day_type(), as.integer(input$visit_hour)
        )
      })
      
      stops <- shiny::reactive({
        route_planner_stops(
          plan(), places, crowd_snapshot(), facilities,
          weather = input$weather, mood = input$crowd,
          comfort = input$comfort,
          start_id = if (is.null(input$start_at)) "auto" else input$start_at
        )
      })
      
      focus_stop <- function(i) {
        p <- shiny::isolate(stops())
        
        if (length(i) != 1 ||
            !is.finite(i) ||
            i < 1 ||
            i > nrow(p)) {
          return(invisible(NULL))
        }
        
        selected_stop(as.integer(i))
        
        leaflet::leafletProxy(
          "route_map",
          session = session
        ) |>
          leaflet::setView(
            lng = p$lng[i],
            lat = p$lat[i],
            zoom = 15
          )
      }
      
      shiny::observeEvent(
        list(
          input$weather,
          input$crowd,
          input$comfort,
          input$start_at,
          input$visit_hour,
          context$month(),
          context$day_type()
        ),
        {
          selected_stop(1L)
          touring(FALSE)
          
          shiny::updateActionButton(
            session,
            "tour_btn",
            label = "▶ Play guided tour"
          )
        },
        ignoreInit = TRUE
      )
      
      output$route_card <- shiny::renderUI({
        r <- plan()
        p <- stops()
        
        segments <- if (nrow(p) > 1) {
          route_planner_distance(
            head(p$lat, -1),
            head(p$lng, -1),
            tail(p$lat, -1),
            tail(p$lng, -1)
          )
        } else {
          0
        }
        
        distance <- round(
          sum(segments),
          1
        )
        readable_story <- sprintf(
          "Begin at %s and finish at %s. Explore %d places at your own pace.",
          p$name[1], p$name[nrow(p)], nrow(p)
        )
        nearby <- p[p$measured & is.finite(p$sample_days), , drop = FALSE]
        reason <- paste0(
          "We compared nearby pedestrian activity from 2024–2025 ",
          "and balanced it with travel distance, weather, ",
          if (input$comfort == "yes") "nearby essentials, " else "",
          "and opening information. ",
          if (input$crowd == "quiet") {
            "Quieter areas received priority."
          } else {
            "Livelier areas received priority."
          }
        )
        
        shiny::tagList(
          shiny::div(
            class = "rp-match",
            "A JOURNEY CHOSEN FOR YOU"
          ),
          
          shiny::tags$h3(
            r$title
          ),
          
          shiny::tags$p(
            class = "rp-subtitle",
            r$subtitle
          ),
          
          shiny::div(
            class = "rp-metrics",
            
            shiny::div(
              shiny::tags$strong(
                nrow(p)
              ),
              shiny::tags$span(
                "Places to visit"
              )
            ),
            
            shiny::div(
              shiny::tags$strong(
                paste0(distance, " km")
              ),
              shiny::tags$span(
                "Point-to-point"
              )
            )
          ),
          
          shiny::tags$h4(
            "A little about your day"
          ),
          
          shiny::tags$p(
            class = "rp-story",
            readable_story
          ),
          
          shiny::tags$h4(
            "Why we chose this journey"
          ),
          
          shiny::tags$p(
            class = "rp-reason",
            reason
          ),
          shiny::tags$p(
            class = "rp-footnote",
            paste0(
              nrow(nearby), " of ", nrow(p),
              " stops have nearby historical crowd estimates. ",
              "These are not live counts or verified walking paths."
            )
          )
        )
      })
      
      output$stop_list <- shiny::renderUI({
        p <- stops()
        
        shiny::tagList(
          lapply(
            seq_len(nrow(p)),
            function(i) {
              
              shiny::tags$button(
                type = "button",
                
                class = paste(
                  "rp-stop",
                  if (i == selected_stop()) {
                    "rp-stop-active"
                  } else {
                    ""
                  }
                ),
                
                onclick = sprintf(
                  paste0(
                    "Shiny.setInputValue(",
                    "'%s', %d, {priority:'event'})"
                  ),
                  session$ns("chosen_stop"),
                  i
                ),
                
                shiny::tags$img(
                  class = "rp-stop-photo",
                  src = p$photo[i],
                  alt = paste("Photograph of", p$name[i]),
                  loading = "lazy"
                ),
                
                shiny::tags$span(
                  class = "rp-stop-content",
                  
                  shiny::tags$strong(
                    p$name[i]
                  ),
                  
                  shiny::tags$small(
                    p$category[i]
                  ),

                  shiny::tags$span(
                    class = paste0("rp-crowd-chip rp-crowd-", p$crowd_band[i]),
                    p$crowd_label[i]
                  ),
                  
                  shiny::tags$span(
                    class = "rp-stop-description",
                    p$short_description[i]
                  ),
                  if (p$opening_status[i] == "closed") {
                    shiny::tags$span(
                      class = "rp-hours-warning",
                      "Closed at selected hour"
                    )
                  }
                )
              )
            }
          )
        )
      })
      
      output$tour_status <- shiny::renderUI({
        p <- stops()
        i <- selected_stop()
        
        shiny::req(
          nrow(p) > 0,
          i >= 1,
          i <= nrow(p)
        )
        
        progress <- round(
          i / nrow(p) * 100
        )
        
        shiny::div(
          class = "rp-live-tour",
          
          shiny::div(
            class = "rp-live-top",
            
            shiny::div(
              shiny::tags$span(
                class = "rp-live-step",
                paste0(
                  "STOP ",
                  i,
                  " OF ",
                  nrow(p)
                )
              ),
              
              shiny::tags$h3(
                p$name[i]
              )
            ),
            
            shiny::div(
              class = "rp-live-state",
              if (isTRUE(touring())) {
                "● Playing"
              } else {
                "● Ready to explore"
              }
            )
          ),
          
          shiny::tags$p(
            class = "rp-live-description",
            p$short_description[i]
          ),
          
          shiny::div(
            class = "rp-progress-track",
            
            shiny::div(
              class = "rp-progress-fill",
              style = paste0(
                "width:",
                progress,
                "%;"
              )
            )
          )
        )
      })
      
      shiny::observeEvent(input$chosen_stop, {
        i <- as.integer(
          input$chosen_stop
        )
        
        touring(FALSE)
        
        shiny::updateActionButton(
          session,
          "tour_btn",
          label = "▶ Play guided tour"
        )
        
        focus_stop(i)
      })
      
      shiny::observeEvent(input$prev_btn, {
        touring(FALSE)
        
        shiny::updateActionButton(
          session,
          "tour_btn",
          label = "▶ Play guided tour"
        )
        
        i <- max(
          1L,
          selected_stop() - 1L
        )
        
        focus_stop(i)
      })
      
      shiny::observeEvent(input$next_btn, {
        touring(FALSE)
        
        shiny::updateActionButton(
          session,
          "tour_btn",
          label = "▶ Play guided tour"
        )
        
        i <- min(
          nrow(stops()),
          selected_stop() + 1L
        )
        
        focus_stop(i)
      })
      
      output$route_map <- leaflet::renderLeaflet({
        p <- stops()
        
        map <- leaflet::leaflet(
          options = leaflet::leafletOptions(
            minZoom = 13.5,
            maxZoom = 18,
            zoomSnap = 0.25,
            preferCanvas = FALSE
          )
        )

        # Match the main map's pan limits; the polygon is the shared
        # approximate study boundary, not the official gazetted FTZ.
        ring <- project$boundary$features[[1]]$geometry$coordinates[[1]]
        xy <- do.call(rbind, lapply(ring, unlist))
        map <- leaflet::setMaxBounds(
          map,
          min(xy[, 1]), min(xy[, 2]),
          max(xy[, 1]), max(xy[, 2])
        )
        
        map <- leaflet::addGeoJSON(
          map,
          project$basemap$parks,
          color = "#c7ddc3",
          weight = 0.6,
          fillColor = "#dcebd7",
          fillOpacity = 1,
          options = leaflet::pathOptions(
            interactive = FALSE
          )
        )
        
        map <- leaflet::addGeoJSON(
          map,
          project$basemap$water,
          color = "#bed8e8",
          weight = 0.7,
          fillColor = "#d8ecf6",
          fillOpacity = 1,
          options = leaflet::pathOptions(
            interactive = FALSE
          )
        )
        
        map <- leaflet::addGeoJSON(
          map,
          project$basemap$roads,
          color = "#cbd3ca",
          weight = 2.4,
          fill = FALSE,
          options = leaflet::pathOptions(
            interactive = FALSE
          )
        )
        
        map <- leaflet::addGeoJSON(
          map,
          project$basemap$roads,
          color = "#ffffff",
          weight = 1.2,
          fill = FALSE,
          options = leaflet::pathOptions(
            interactive = FALSE
          )
        )
        
        boundary_file <- file.path(
          .route_planner_root, "data",
          "ftz_boundary_2025.geojson"
        )
        
        if (file.exists(boundary_file)) {
          boundary_json <- paste(
            readLines(
              boundary_file,
              warn = FALSE,
              encoding = "UTF-8"
            ),
            collapse = "\n"
          )
          
          map <- leaflet::addGeoJSON(
            map,
            boundary_json,
            color = "#347c80",
            weight = 3,
            dashArray = "8,6",
            fillColor = "#91c7c2",
            fillOpacity = 0.035,
            options = leaflet::pathOptions(
              interactive = FALSE
            )
          )
        }
        
        if (nrow(p) > 1) {
          map <- leaflet::addPolylines(
            map,
            lng = p$lng,
            lat = p$lat,
            color = "#326bbf",
            weight = 5,
            opacity = 0.85,
            dashArray = "10,8",
            options = leaflet::pathOptions(
              interactive = FALSE
            )
          )
        }
        
        # Display the same local attraction photographs on the map as in
        # the journey cards. The images are bundled in app/www/route-photos.
        place_icons <- leaflet::icons(
          iconUrl = p$photo,
          iconWidth = 62,
          iconHeight = 48,
          iconAnchorX = 31,
          iconAnchorY = 24,
          popupAnchorY = -30
        )
        
        map <- leaflet::addMarkers(
          map,
          lng = p$lng,
          lat = p$lat,
          icon = place_icons,
          layerId = paste0(
            "stop_",
            p$number
          ),
          label = paste0(
            p$name
          ),
          popup = paste0(
            "<div class='rp-popup rp-popup-photo-card'>",
            "<img class='rp-popup-photo' src='",
            htmltools::htmlEscape(p$photo),
            "' alt='Photograph of ",
            htmltools::htmlEscape(p$name),
            "'>",
            "<strong>",
            htmltools::htmlEscape(
              p$name
            ),
            "</strong><p>",
            htmltools::htmlEscape(
              p$short_description
            ),
            "</p></div>"
          )
        )
        
        if (!is.null(facilities)) {
          near <- vapply(
            seq_len(nrow(facilities)),
            function(i) {
              min(
                route_planner_distance(
                  facilities$latitude[i],
                  facilities$longitude[i],
                  p$lat,
                  p$lng
                )
              )
            },
            numeric(1)
          )
          
          nearby <- facilities[
            near <= 0.4,
            ,
            drop = FALSE
          ]
          
          if (isTRUE(input$show_toilets)) {
            toilets <- nearby[
              nearby$facility_type == "toilet",
              ,
              drop = FALSE
            ]
            
            if (nrow(toilets) > 0) {
              map <- leaflet::addLabelOnlyMarkers(
                map,
                lng = toilets$longitude,
                lat = toilets$latitude,
                
                label = lapply(
                  seq_len(nrow(toilets)),
                  function(i) {
                    htmltools::HTML(
                      "<span class='rp-wc-icon'>WC</span>"
                    )
                  }
                ),
                
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = "center",
                  textOnly = TRUE,
                  className = "rp-facility-label"
                ),
                
                options = leaflet::markerOptions(
                  interactive = FALSE
                )
              )
            }
          }
          
          if (isTRUE(input$show_access)) {
            accessible <- nearby[
              nearby$wheelchair_raw == "yes",
              ,
              drop = FALSE
            ]
            
            if (nrow(accessible) > 0) {
              map <- leaflet::addLabelOnlyMarkers(
                map,
                lng = accessible$longitude,
                lat = accessible$latitude,
                
                label = lapply(
                  seq_len(nrow(accessible)),
                  function(i) {
                    htmltools::HTML(
                      "<span class='rp-access-icon'>♿</span>"
                    )
                  }
                ),
                
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = "center",
                  textOnly = TRUE,
                  className = "rp-facility-label"
                ),
                
                options = leaflet::markerOptions(
                  interactive = FALSE
                )
              )
            }
          }
        }
        
        leaflet::fitBounds(
          map,
          lng1 = min(p$lng) - 0.004,
          lat1 = min(p$lat) - 0.003,
          lng2 = max(p$lng) + 0.004,
          lat2 = max(p$lat) + 0.003
        )
      })
      
      shiny::observeEvent(
        input$route_map_marker_click,
        {
          click <- input$route_map_marker_click
          
          if (is.null(click$id)) {
            return()
          }
          
          if (!grepl(
            "^stop_[0-9]+$",
            click$id
          )) {
            return()
          }
          
          i <- as.integer(
            sub(
              "stop_",
              "",
              click$id
            )
          )
          
          if (i >= 1 &&
              i <= nrow(stops())) {
            selected_stop(i)
            touring(FALSE)
            
            shiny::updateActionButton(
              session,
              "tour_btn",
              label = "▶ Play guided tour"
            )
          }
        }
      )
      
      shiny::observe({
        shiny::req(
          input$route_map_bounds
        )
        
        p <- stops()
        i <- selected_stop()
        
        shiny::req(
          nrow(p) > 0,
          i >= 1,
          i <= nrow(p)
        )
        
        map <- leaflet::leafletProxy(
          "route_map",
          session = session
        )
        
        map <- leaflet::clearGroup(
          map,
          "rp_tour_progress"
        )
        
        if (i > 1) {
          map <- leaflet::addPolylines(
            map,
            lng = p$lng[seq_len(i)],
            lat = p$lat[seq_len(i)],
            color = "#f49b38",
            weight = 8,
            opacity = 0.95,
            group = "rp_tour_progress",
            options = leaflet::pathOptions(
              interactive = FALSE
            )
          )
        }
        
        leaflet::addCircleMarkers(
          map,
          lng = p$lng[i],
          lat = p$lat[i],
          radius = 22,
          color = "#f49b38",
          weight = 5,
          opacity = 0.95,
          fillColor = "#fbdba9",
          fillOpacity = 0.2,
          group = "rp_tour_progress",
          options = leaflet::pathOptions(
            interactive = FALSE
          )
        )
      })
      
      shiny::observeEvent(input$tour_btn, {
        active <- !isTRUE(
          touring()
        )
        
        if (active) {
          focus_stop(1L)
        }
        
        touring(active)
        
        shiny::updateActionButton(
          session,
          "tour_btn",
          label = if (active) {
            "Ⅱ Pause guided tour"
          } else {
            "▶ Play guided tour"
          }
        )
      })
      
      shiny::observe({
        if (!isTRUE(touring())) {
          return()
        }
        
        shiny::invalidateLater(
          3500,
          session
        )
        
        shiny::isolate({
          p <- stops()
          i <- selected_stop() + 1L
          
          if (i > nrow(p)) {
            touring(FALSE)
            
            shiny::updateActionButton(
              session,
              "tour_btn",
              label = "▶ Play guided tour"
            )
            
            return()
          }
          
          focus_stop(i)
        })
      })
    }
  )
}
