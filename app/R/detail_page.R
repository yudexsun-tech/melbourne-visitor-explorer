# Resolve local resources when the module is sourced, before reactive callbacks
# can run under a different RStudio/Shiny working directory.
.detail_page_assets <- local({
  app_root <- local({
  source_files <- vapply(sys.frames(), function(frame) {
    path <- frame$ofile
    if (is.character(path) && length(path) == 1L) path else ""
  }, character(1))
  module_file <- tail(source_files[basename(source_files) == "detail_page.R"], 1L)
  normalizePath(if (length(module_file)) file.path(dirname(module_file), "..") else ".",
                winslash = "/", mustWork = TRUE)
  })
  read_asset <- function(...) paste(readLines(file.path(app_root, "www", ...),
    warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  kinds <- c("toilet", "drinking_water", "bench", "shelter")
  list(initializer = read_asset("detail_map.js"),
       icons = setNames(lapply(kinds, function(kind) paste(readLines(
         file.path(app_root, "www", "detail-icons", paste0(kind, ".svg")),
         warn = FALSE, encoding = "UTF-8"), collapse = " ")), kinds))
})

# Visitor-facing copy; keep source wording in the archived data.
detail_visit_text <- function(value) {
  short <- c(
    "No single venue timetable applies to the selected public-space or exterior scope." = "No fixed opening hours.",
    "Public-space or exterior viewing is the selected scope. Paid venues, purchases, tours and transport are excluded." = "Free to visit.",
    "Self-guided public-space viewing is the selected scope. Separate events and tours require their own checks." = "No booking needed.",
    "Martin Grant exhibition tickets were sold online, by phone and onsite. Wheelchair reservation was recommended. The ordinary free-collection advance-booking policy was not explicitly established." = "No booking needed for the collection. Ticketed exhibitions and wheelchair loans have separate arrangements.",
    "Street-level automatic-door entrances, lifts to each floor and accessible toilets on all levels. Free wheelchairs can be reserved at the Information Desk. No down escalator at NGV Australia. Scooter hire is not confirmed for this venue." = "Street-level automatic-door entrances, lifts to each floor and accessible toilets on all levels. Free wheelchairs can be reserved at the Information Desk. No down escalator at NGV Australia. Scooter hire: Not available.",
    "Order Up tickets were sold online and onsite in the 6 October announcement. Advance reservation of borrowed wheelchairs or a scooter was recommended. General admission advance-booking necessity or exemption remains unconfirmed." = "Tickets can be purchased online before arrival. Wheelchair and mobility scooter reservations are recommended.",
    "Adult AUD 15; children up to 16, eligible concessions and members free. Australian Seniors Card holders: AUD 10 on 3 February; AUD 12 on 7 December. Exact change date unknown; older overseas visitors do not qualify by age alone. Some programmes cost extra." = "Adult AUD 15; children up to 16, eligible concessions and members free. Australian Seniors Card holders: AUD 10 on 3 February; AUD 12 on 7 December. Some programmes cost extra.",
    "2025 sources differ: Jan 14 and Nov 4 pages list Mon-Sat 10:00-17:00; Apr 20 booklet lists weekdays 10:00-17:00, Sat 10:00-16:00; Oct 26 booklet lists weekdays 10:00-18:00, Sat 10:00-17:00. Booklets list Sundays during services. Change dates and full exceptions are unknown." = "Monday to Friday: 10:00-18:00. Saturday: 10:00-17:00. Sunday: 08:00-18:00. Sightseeing is not permitted while a church service is taking place.",
    "The City Flash Forward listing identifies the Royal Arcade site as wheelchair accessible and welcoming guide/service animals. Access to every tenant, upper floor and public toilet is not established." = "Wheelchair accessible. Guide and service animals welcome.",
    "The archived Visit Victoria listing identifies access for visitors who remain seated in a wheelchair. It does not specify every entrance, tenant or public toilet." = "Access for visitors who remain seated in a wheelchair.",
    "The archived City guide describes a cobbled laneway. Wheelchair access, step-free approaches and gradients are not specified." = "Cobbled laneway. Wheelchair access: Not available.",
    "Public visitor spaces in the library; the front lawn is excluded from this attraction record." = "Public visitor spaces in the library.",
    "NGV Australia at Federation Square; NGV International on St Kilda Road is excluded." = "NGV Australia at Federation Square.",
    "ACMI public exhibition spaces; individual exhibitions and events require separate checking." = "ACMI public exhibition spaces.",
    "Primarily the cathedral interior; worship and event times require separate checking." = "Cathedral interior.",
    "Public market spaces and covered market passages; surrounding streets are not all included." = "Public market spaces and covered passages.",
    "Public arcade passage; shops are not separate attractions." = "Public arcade passage.",
    "Bourke Street between Swanston and Elizabeth Streets; department-store interiors are excluded." = "Bourke Street between Swanston and Elizabeth Streets.",
    "Little Bourke Street between Swanston and Exhibition Streets; restaurants are not separate attractions." = "Little Bourke Street between Swanston and Exhibition Streets.",
    "Exterior and public area near the main entrance and clocks at Flinders and Swanston Streets; paid platforms are excluded." = "Exterior and public area near the entrance clocks at Flinders and Swanston Streets.",
    "Federation Square public plaza; ACMI and NGV Australia are separate attractions, so their interior facilities should not be double-counted." = "Federation Square public plaza.",
    "Entrance clocks, yellow facade and green dome. Paid platforms are excluded." = "Entrance clocks, yellow facade and green dome.",
    "Public plaza and surrounding architecture. Museum interiors are separate attractions." = "Public plaza and surrounding architecture.",
    "ACMI venue reference point; a 2025 entrance coordinate has not been verified" = "ACMI map location",
    "Arcade front reference near Bourke Street; the 2025 node has no entrance tag" = "Arcade map location near Bourke Street",
    "Arcade front reference near Collins Street; the 2025 node has no entrance tag" = "Arcade map location near Collins Street",
    "West gateway at Little Bourke and Swanston Streets; the point does not represent foot traffic along the whole precinct" = "West gateway at Little Bourke and Swanston Streets"
  )
  i <- match(as.character(value), names(short))
  if (length(i) == 1L && !is.na(i)) unname(short[i]) else value
}

# Visitor information checked against official sources on 2026-10-30.
# Source URLs and evidence basis are retained in metadata/visitor_visit_updates_2026-10-30.json.
detail_visit_field <- function(attraction_id, field, value) {
  updates <- list(
    ATTR006 = c(booking_notes_en = "No booking needed for self-guided visits."),
    ATTR007 = c(admission_notes_en = "Free to visit.", booking_notes_en = "Walk in during opening hours."),
    ATTR008 = c(opening_hours = "Monday to Thursday: 08:00-18:00. Friday: 08:00-20:00. Saturday: 08:00-18:00. Sunday: 09:00-18:00. Individual shops can differ.", admission_notes_en = "Free to visit.", booking_notes_en = "Walk in during opening hours.")
  )
  update <- updates[[as.character(attraction_id)]]
  if (!is.null(update) && field %in% names(update)) return(unname(update[[field]]))
  value <- detail_visit_text(value)
  if (field == "opening_hours") detail_opening_hours_text(value) else value
}

detail_content <- list()

detail_missing <- "Not available"
detail_value <- function(x, missing = detail_missing) {
  if (!length(x) || is.na(x[1])) return(missing)
  value <- trimws(as.character(detail_visit_text(x[1])))
  if (tolower(value) %in% c("", "unknown", "unavailable", "not recorded", "not available", "no information", "n/a", "na", "null", "—", "————")) missing else value
}
detail_has_value <- function(x) nzchar(detail_value(x, ""))
detail_types <- c(toilet = "Toilets", drinking_water = "Drinking water", bench = "Benches", shelter = "Shelters")
detail_labels <- c(toilet = "Toilet", drinking_water = "Drinking water", bench = "Bench", shelter = "Shelter")
detail_fact <- function(label, value, optional = FALSE) {
  if (isTRUE(optional) && !detail_has_value(value)) return(NULL)
  tagList(tags$dt(label), tags$dd(detail_value(value)))
}
detail_icon <- function(kind, label = "", class = "detail-icon") {
  tags$img(src = paste0("detail-icons/",kind,".svg"), alt = label,
    class = class, width = 22, height = 22, `aria-hidden` = if(!nzchar(label)) "true" else NULL)
}
detail_facility_number_label <- function(number) sprintf("%02d",as.integer(number))
detail_facility_marker_url <- local({
  templates<-new.env(parent=emptyenv());urls<-new.env(parent=emptyenv())
  function(kind,number){
    stopifnot(length(kind)==1L,kind%in%names(detail_types),length(number)==1L,is.finite(number),number>=1)
    label<-detail_facility_number_label(number);key<-paste(kind,label,sep=":")
    if(exists(key,envir=urls,inherits=FALSE))return(get(key,envir=urls))
    if(!exists(kind,envir=templates,inherits=FALSE)){
      raw<-.detail_page_assets$icons[[kind]]
      body<-sub("^.*?<svg[^>]*>","",raw,perl=TRUE)
      body<-sub("</svg>\\s*$","",body,perl=TRUE)
      assign(kind,body,envir=templates)
    }
    svg<-paste0('<svg xmlns="http://www.w3.org/2000/svg" width="36" height="34" viewBox="0 0 36 34">',
      '<svg x="0" y="0" width="24" height="24" viewBox="0 0 24 24">',get(kind,envir=templates),'</svg>',
      '<rect x="10" y="19" width="25" height="14" rx="4" fill="#fffffc" stroke="#dbe4d2"/>',
      '<text x="22.5" y="29" text-anchor="middle" font-family="Arial,sans-serif" font-size="9" font-weight="500" fill="#213c33">',label,'</text></svg>')
    url<-paste0("data:image/svg+xml;base64,",gsub("[[:space:]]","",jsonlite::base64_enc(charToRaw(svg))))
    assign(key,url,envir=urls);url
  }
})
detail_setting_badge <- function(project, attraction_id) {
  if(length(attraction_id)!=1L||is.na(attraction_id)||is.null(project$detail$descriptions))return(NULL)
  r<-project$detail$descriptions
  r<-r[r$attraction_id==attraction_id,,drop=FALSE]
  if(nrow(r)!=1L)return(NULL)
  labels<-c(indoor="Indoor",outdoor="Outdoor",covered="Covered",mixed="Mixed")
  value<-tolower(detail_value(r$setting,""))
  if(!value%in%names(labels))return(NULL)
  tags$span(class="detail-setting-badge",unname(labels[value]))
}
detail_safe_link <- function(url, label = url) {
  url <- detail_value(url, "")
  if (!grepl("^https?://", url)) return(NULL)
  tags$a(href = url, target = "_blank", rel = "noopener noreferrer", label)
}
detail_section <- function(title, ...) div(class = "detail-section", tags$h2(title), ...)
detail_click <- function(id, value) sprintf("Shiny.setInputValue(%s,%s,{priority:'event'});",
  jsonlite::toJSON(id, auto_unbox = TRUE), jsonlite::toJSON(value, auto_unbox = TRUE))
detail_svg_point <- function(ns, hour, x, y, title, selected = FALSE) {
  tags$circle(cx = round(x, 2), cy = round(y, 2), r = if (selected) 5 else 3.5,
    fill = if (selected) "#213c33" else "#fffffc", stroke = "#54765b", `stroke-width` = 1.5,
    role = "button", tabindex = "0", `aria-label` = title,
    onclick = detail_click(ns("chart_hour"), hour),
    onkeydown = paste0("if(event.key==='Enter'||event.key===' '){event.preventDefault();", detail_click(ns("chart_hour"), hour), "}"),
    tags$title(title))
}
detail_line_svg <- function(rows, ns, hour, threshold, statistic = "Mean", moderate_threshold = NA_real_) {
  valid <- is.finite(rows$mean)
  if (!any(valid)) return(tags$p(class = "detail-empty", detail_missing))
  w <- 700; h <- 215; left <- 55; right <- 18; top <- 18; bottom <- 38
  ymax <- max(c(rows$p75, rows$mean, threshold), na.rm = TRUE) * 1.08
  x <- function(v) left + v / 23 * (w-left-right)
  y <- function(v) h-bottom - v / ymax * (h-top-bottom)
  fmt <- function(v) paste(sprintf("%.2f,%.2f", x(rows$hour[v]), y(rows$mean[v])), collapse = " ")
  runs <- split(which(valid), cumsum(c(TRUE, diff(which(valid)) != 1)))
  band_ids <- which(valid & is.finite(rows$p25) & is.finite(rows$p75))
  band_runs <- if(length(band_ids)) split(band_ids, cumsum(c(TRUE, diff(band_ids) != 1))) else list()
  bands <- lapply(band_runs, function(i) {
    if (!length(i)) return(NULL)
    points <- paste(sprintf("%.2f,%.2f", c(x(rows$hour[i]), rev(x(rows$hour[i]))),
      c(y(rows$p25[i]), rev(y(rows$p75[i])))), collapse = " ")
    tags$polygon(points = points, fill = "#e2ecd8", stroke = "none")
  })
  ticks <- pretty(c(0, ymax), n = 4); ticks <- ticks[ticks >= 0 & ticks <= ymax]
  tags$svg(viewBox = sprintf("0 0 %d %d", w, h), role = "group", `aria-label` = "Hourly street foot traffic. Select a point to change the shared map hour.",
    style = "width:100%;height:auto;display:block;overflow:visible",
    lapply(ticks, function(v) tagList(tags$line(x1 = left, x2 = w-right, y1 = y(v), y2 = y(v), stroke = "#e5e8df"),
      tags$text(x = left-8, y = y(v)+3, `text-anchor` = "end", `font-size` = 10, fill = "#7a837a", format_count(round(v))))),
    bands,
    if (is.finite(moderate_threshold)) tagList(
      tags$line(x1 = left, x2 = w-right, y1 = y(moderate_threshold), y2 = y(moderate_threshold), stroke = "#e0b323", `stroke-dasharray` = "4 4", `stroke-width` = 1),
      tags$text(x = w-right, y = y(moderate_threshold)-5, `text-anchor` = "end", `font-size` = 9, fill = "#8a6b00", paste("Moderate threshold", round_crowd_count(moderate_threshold)))),
    tags$line(x1 = left, x2 = w-right, y1 = y(threshold), y2 = y(threshold), stroke = "#c62828", `stroke-dasharray` = "4 4", `stroke-width` = 1),
    tags$text(x = w-right, y = y(threshold)-5, `text-anchor` = "end", `font-size` = 9, fill = "#c62828", paste("Busy threshold", round_crowd_count(threshold))),
    lapply(runs, function(i) tags$polyline(points = fmt(i), fill = "none", stroke = "#54765b", `stroke-width` = 2)),
    tags$line(x1 = x(hour), x2 = x(hour), y1 = top, y2 = h-bottom, stroke = "#8b9290", `stroke-dasharray` = "2 4"),
    lapply(which(valid), function(i) detail_svg_point(ns, rows$hour[i], x(rows$hour[i]), y(rows$mean[i]),
      sprintf("%02d:00. %s %.1f street passages/hour. Middle 50%% %.1f to %.1f. %d valid observations.",
        rows$hour[i], statistic, round_crowd_count(rows$mean[i]), round_crowd_count(rows$p25[i]), round_crowd_count(rows$p75[i]), rows$n[i]), rows$hour[i] == hour)),
    lapply(c(0, 6, 12, 18, 23), function(v) tags$text(x = x(v), y = h-18, `text-anchor` = "middle", `font-size` = 10, fill = "#7a837a", sprintf("%02d", v))),
    tags$text(x = left, y = 10, `font-size` = 9, fill = "#7a837a", paste(statistic, "street passages / hour")),
    tags$text(x = w-right, y = h-2, `text-anchor` = "end", `font-size` = 9, fill = "#7a837a", "Local hour"))
}
detail_week_svg <- function(rows, ns, weekday, hour) {
  if (!nrow(rows)) return(tags$p(class = "detail-empty", detail_missing))
  left <- 55; cell <- 25; cellh <- 22
  tags$svg(viewBox = "0 0 675 190", role = "group", `aria-label` = "Full week of hourly means for the selected month and weather in 2024–2025. Select a cell to show that weekday and hour in the daily profile.", style = "width:100%;height:auto;display:block",
    lapply(1:7, function(d) tags$text(x = 48, y = 20+(d-1)*cellh+13, `text-anchor` = "end", `font-size` = 10, fill = "#7a837a", c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")[d])),
    lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      closed <- identical(r$opening_status, "closed")
      unknown <- identical(r$opening_status, "unknown")
      missing <- unknown || !is.finite(r$mean_pedestrian_count) || !r$crowd_class %in% c("Quiet", "Moderate", "Crowded")
      label <- if ("display_class" %in% names(r)) r$display_class else r$crowd_class
      title <- if (closed) sprintf("%s %02d:00. Closed.", r$weekday_name, r$hourday) else
        if (unknown) sprintf("%s %02d:00. Opening hours: %s.", r$weekday_name, r$hourday, detail_missing) else
        if (missing) sprintf("%s %02d:00. %sFoot traffic: %s.", r$weekday_name, r$hourday,
          if (identical(r$opening_status, "open")) "Open. " else "", detail_missing) else
          sprintf("%s %02d:00. Mean %.1f street passages/hour. %s. %d observations.",
            detail_value(r$weekday_name), r$hourday, round_crowd_count(r$mean_pedestrian_count), detail_value(label), r$n_observed)
      action <- detail_click(ns("week_cell"), list(weekday = r$weekday_iso, hour = r$hourday))
      tags$rect(x = left+r$hourday*cell, y = 20+(r$weekday_iso-1)*cellh, width = cell-2, height = cellh-2,
        fill = if (closed) "#8b9290" else if (missing) "none" else unname(c(Quiet = "#2e7d32", Moderate = "#e0b323", Crowded = "#c62828")[r$crowd_class]),
        stroke = if (r$weekday_iso == weekday && r$hourday == hour) "#213c33" else if (missing && !closed) "#b8c0b2" else "none", `stroke-width` = 2,
        role = "button", tabindex = "0", `aria-label` = title, onclick = action,
        onkeydown = paste0("if(event.key==='Enter'||event.key===' '){event.preventDefault();", action, "}"), tags$title(title))
    }), lapply(c(0,6,12,18,23), function(v) tags$text(x = left+v*cell+10, y = 185, `text-anchor` = "middle", `font-size` = 10, fill = "#7a837a", sprintf("%02d",v))))
}

attraction_detail_ui <- function(attraction_id, attraction_row, ns = identity, project = NULL, settings = list()) {
  div(class="detail-content",id="detail-content",
    div(class="detail-workspace",
      div(class="detail-main",
        div(class="detail-map",leafletOutput(ns("nearby_map"),height="100%")),
        div(class="detail-inline-legend detail-map-legend",
          lapply(names(detail_labels),function(kind)tags$span(detail_icon(kind),unname(detail_labels[kind]))),
          tags$span(detail_icon("entrance"),"Entrance / reference"),
          tags$span(tags$span(class="detail-facility-number","01"),"Facility reference")),
        div(class="detail-section",uiOutput(ns("context_summary")),
          tags$p(class="detail-note detail-facility-hint detail-category-hint","Select a category to filter facilities."),
          uiOutput(ns("facility_counts")),
          tags$h3("Nearest essentials"),
          tags$p(class="detail-note detail-facility-hint","Select a card to locate the facility."),
          uiOutput(ns("nearest_essentials"))),
        div(class="detail-tabs",tabsetPanel(id=ns("info_tab"),type="pills",
          tabPanel("Overview",uiOutput(ns("overview")),uiOutput(ns("access"))),
          tabPanel("Transport",uiOutput(ns("transport")))))),
      tags$aside(class="attractions-panel detail-sidebar",
        div(class="transport-panel detail-controls",
          tags$p(class="eyebrow","AROUND THIS ATTRACTION"),tags$h2("Nearby facilities"),
          selectInput(ns("radius"),"Nearby radius",c("300 m"=300,"500 m"=500,"1000 m"=1000),300,selectize=FALSE),
          checkboxGroupInput(ns("facility_types"),"Facility types",
            choiceNames=lapply(names(detail_types),function(kind)tagList(detail_icon(kind),unname(detail_types[kind]))),
            choiceValues=names(detail_types),selected=names(detail_types)),
          checkboxGroupInput(ns("needs"),"Accessibility",c("Wheelchair-accessible facilities"="wheelchair")),
          selectInput(ns("sort"),"Sort facilities",c("Nearest first"="distance","Type, then distance"="type"),selectize=FALSE),
          actionButton(ns("reset_facilities"),"Reset facility filters",class="transport-reset"),
          textOutput(ns("facility_status"),container=function(...)tags$p(class="nearby-status",...)),
          uiOutput(ns("facility_list"),class="detail-facility-list"),uiOutput(ns("facility_detail"))))),
    div(class="detail-section detail-time-section",
      div(class="detail-time-heading",tags$p(class="eyebrow","FOOT TRAFFIC"),uiOutput(ns("visiting_time_title"))),
      tags$p(class="detail-note","Click a point or a weekday/hour cell to change the visiting time."),
      div(class="detail-chart-controls",
        selectInput(ns("profile_basis"),"Daily profile",c("Main map selection · mean"="filtered","Selected weekday · mean"="typical"),selectize=FALSE,width="260px"),
        conditionalPanel(condition="input.profile_basis === 'typical'",ns=ns,
          selectInput(ns("weekday"),"Weekday",setNames(1:7,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),1,selectize=FALSE,width="160px"))),
      uiOutput(ns("profile_note")),uiOutput(ns("profile_legend")),
      uiOutput(ns("hour_profile"),class="detail-chart"),uiOutput(ns("selected_hour")),
      tags$h3("Weekday / hour · full week"),uiOutput(ns("week_profile"),class="detail-chart"),
      div(class="detail-inline-legend",tags$span(tags$i(class="legend-dot quiet"),"Quiet"),tags$span(tags$i(class="legend-dot moderate"),"Moderate"),tags$span(tags$i(class="legend-dot crowded"),"Busy"),tags$span(tags$i(class="legend-dot unknown"),"Closed"),tags$span(tags$i(class="legend-dot no-data"),"Not available")),
      tags$p(class="detail-note","Full week for the selected month and weather. Select a cell to view that weekday’s hourly profile."),
      uiOutput(ns("summary_cards"))),
    if(is.function(detail_content[[attraction_id]]))detail_content[[attraction_id]](attraction_row,ns))
}

attraction_detail_server <- function(id, context) {
  moduleServer(id, function(input, output, session) {
    p <- context$project; ns <- session$ns
    selected_facility <- reactiveVal(NULL)
    focused_entry <- reactiveVal(NULL)
    focus_request <- reactiveVal(0L)
    row <- reactive({req(context$attraction_id()); p$detail$descriptions[p$detail$descriptions$attraction_id==context$attraction_id(),,drop=FALSE]})
    radius <- reactive({v<-suppressWarnings(as.numeric(input$radius)); if(length(v)!=1||is.na(v)||!v%in%c(300,500,1000)) 300 else v})
    needs <- reactive(intersect(as.character(input$needs),"wheelchair"))
    facilities <- reactive(detail_facilities(p, context$attraction_id(), radius(),
      types=if(is.null(input$radius)&&is.null(input$facility_types)) names(detail_types) else input$facility_types,
      needs=needs(),inside_only=FALSE))
    nearest_essentials <- reactive({
      f<-facilities()
      kinds<-c("toilet","drinking_water","bench")
      setNames(lapply(kinds,function(kind){
        r<-f[f$facility_type==kind,,drop=FALSE]
        r<-r[order(r$distance_m,r$facility_uid),,drop=FALSE]
        head(r,1L)
      }),kinds)
    })
    nearby_stops <- reactive(get_nearby_transport(p,context$attraction_id(),radius()))
    observeEvent(list(context$attraction_id(),input$radius,input$facility_types,input$needs), {selected_facility(NULL);focused_entry(NULL)},ignoreInit=TRUE,priority=10)
    observeEvent(input$reset_facilities, {
      updateCheckboxGroupInput(session,"facility_types",selected=names(detail_types)); updateCheckboxGroupInput(session,"needs",selected=character())
      updateSelectInput(session,"radius",selected=300)
      updateSelectInput(session,"sort",selected="distance"); selected_facility(NULL);focused_entry(NULL)
    })
    observeEvent(input$view_transport,{
      if(is.function(context$show_transport)) context$show_transport()
    })
    observeEvent(input$chart_hour, {v<-suppressWarnings(as.integer(input$chart_hour));if(length(v)==1&&!is.na(v)&&v%in%0:23)context$set_hour(v)})
    observeEvent(input$week_cell, {
      d<-as.integer(input$week_cell$weekday); h<-as.integer(input$week_cell$hour)
      if(length(d)==1&&length(h)==1&&!is.na(d)&&!is.na(h)&&d%in%1:7&&h%in%0:23){
        updateSelectInput(session,"profile_basis",selected="typical");updateSelectInput(session,"weekday",selected=d);context$set_hour(h)}
    })
    output$context_summary <- renderUI({s<-get_monthly_snapshot(p,context$month(),context$weather(),context$day_type(),context$hour());s<-s[s$attraction_id==context$attraction_id(),,drop=FALSE];req(nrow(s)==1)
      div(class="selected-context",tags$h2("Around the attraction"),
        div(class="detail-inline-legend",tags$span(tags$i(class=paste("legend-dot",s$color_key)),if(is.na(s$display_count))detail_missing else detail_value(s$status_zh)),
          tags$span(sprintf("%02d:00 · %s · %s · %s",context$hour(),p$month_labels[[context$month()]],if(context$weather()=="good_weather")"Good weather" else "Bad weather",if(context$day_type()=="holiday")"Holiday" else "Weekdays"))),
        if(is.finite(s$display_count))tags$p(class="detail-note",sprintf("%.1f mean nearby street passages/hour",round_crowd_count(s$display_count))))
    })
    output$facility_counts <- renderUI({f<-detail_facilities(p,context$attraction_id(),radius(),types=names(detail_types),needs=needs(),inside_only=FALSE); counts<-table(factor(f$facility_type,levels=names(detail_types)))
      div(class="detail-metrics",lapply(seq_along(detail_types),function(i){kind<-names(detail_types)[i]
        tags$button(type="button",class=paste("transport-reset detail-metric",if(identical(input$facility_types,kind))"is-selected" else ""),
          onclick=detail_click(ns("category_click"),kind),detail_icon(kind),tags$strong(unname(counts[i])),tags$span(unname(detail_types[i])))}))
    })
    observeEvent(input$category_click,{if(input$category_click%in%names(detail_types))updateCheckboxGroupInput(session,"facility_types",selected=input$category_click)})
    output$nearest_essentials<-renderUI({
      nearest<-nearest_essentials()
      div(class="detail-essentials",lapply(names(nearest),function(kind){
        r<-nearest[[kind]];available<-nrow(r)==1L
        restricted<-available&&isTRUE(r$access_restricted)
        label<-unname(detail_labels[kind])
        tags$button(type="button",class=paste("transport-reset detail-essential-card",if(available&&identical(selected_facility(),r$facility_uid))"is-selected" else ""),
          disabled=if(!available)"disabled" else NULL,
          onclick=if(available)paste0(detail_click(ns("facility_select"),r$facility_uid),
            "var mapEl=document.getElementById(",jsonlite::toJSON(ns("nearby_map"),auto_unbox=TRUE),
            ");if(mapEl){mapEl.scrollIntoView({behavior:'smooth',block:'center'});}") else NULL,
          title=if(available)paste("Locate",label,round(r$distance_m),"m",if(restricted)"· restricted access" else "") else detail_missing,
          detail_icon(kind),tags$span(class="detail-essential-label",label),
          tags$strong(if(available)paste(round(r$distance_m),"m") else detail_missing),
          tags$small(if(!available)detail_missing else paste0("#",detail_facility_number_label(r$facility_number),if(restricted)" · Restricted access · locate" else " · Locate")))
      }))
    })
    output$facility_status <- renderText(sprintf("%d facilities within %d m",nrow(facilities()),radius()))
    output$facility_list <- renderUI({f<-facilities();if(!nrow(f))return(tags$p(class="detail-empty",detail_missing))
      if(identical(input$sort,"type")) f<-f[order(f$facility_type,f$distance_m),,drop=FALSE]
      tagList(lapply(seq_len(nrow(f)),function(i){r<-f[i,];tags$button(type="button",class=paste("attraction-card detail-facility-row",if(identical(selected_facility(),r$facility_uid))"is-selected" else ""),
        onclick=detail_click(ns("facility_select"),r$facility_uid),`aria-label`=paste("Inspect",r$display_name,round(r$distance_m),"metres away"),
        tags$span(class="detail-facility-number",detail_facility_number_label(r$facility_number)),
        tags$span(class="detail-facility-kind",detail_icon(r$facility_type,unname(detail_labels[r$facility_type]))),
        div(class="attraction-copy",tags$strong(r$display_name),div(class="attraction-status",tags$span(class="count-value",paste(round(r$distance_m),"m")),if(isTRUE(r$access_restricted))tags$span("Restricted access"))),tags$span(class="card-arrow","›"))}))
    })
    observeEvent(input$facility_select,{if(input$facility_select%in%facilities()$facility_uid){focused_entry(NULL);selected_facility(input$facility_select);focus_request(focus_request()+1L)}})
    observeEvent(input$nearby_map_marker_click, {
      key<-input$nearby_map_marker_click$id
      if(length(key)==1&&startsWith(key,"facility:")){uid<-substring(key,10);if(uid%in%facilities()$facility_uid){focused_entry(NULL);selected_facility(uid);focus_request(focus_request()+1L)}}
    })
    output$facility_detail <- renderUI({
      f<-facilities();r<-f[f$facility_uid%in%selected_facility(),,drop=FALSE];if(!nrow(r))return(NULL)
      optional<-function(label,value)detail_fact(label,value,optional=TRUE)
      tags$section(class="detail-section",
        tags$h3(class="detail-icon-heading",tags$span(class="detail-facility-number",detail_facility_number_label(r$facility_number)),detail_icon(r$facility_type),r$display_name),
        tags$dl(class="detail-facts",
          detail_fact("Distance",paste(round(r$distance_m),"m")),
          optional("Wheelchair",r$wheelchair_raw),optional("Fee",r$fee_raw),
          optional("Access",r$access_raw),optional("Opening hours",detail_opening_hours_text(r$opening_hours_raw)),
          if(r$facility_type=="toilet")optional("Changing table",r$changing_table_raw),
          if(r$facility_type=="bench")tagList(optional("Backrest",r$backrest_raw),optional("Armrest",r$armrest_raw)),
          if(r$facility_type=="drinking_water")optional("Bottle refill",r$bottle_raw),
          if(r$facility_type=="shelter")optional("Shelter type",r$shelter_type_raw),
          optional("Address",r$address_2025)),
        if(r$facility_type=="shelter")tags$p(class="detail-note","Some shelters are intended for waiting transport passengers."),
        detail_safe_link(r$osm_element_url,"View on OpenStreetMap"))
    })
    # The base widget has no reactive filter dependencies. Each DOM mount sends a
    # ready event; proxies then replay the latest small layers, including on re-entry.
    output$nearby_map <- renderLeaflet({
      m<-make_offline_basemap(p,initialize=FALSE)
      ring<-do.call(rbind,lapply(p$boundary$features[[1]]$geometry$coordinates[[1]],unlist))
      m<-addPolygons(m,lng=ring[,1],lat=ring[,2],color="#738a79",weight=1.2,dashArray="5 6",fillOpacity=.02,group="FTZ",options=pathOptions(interactive=FALSE))
      m<-setMaxBounds(m,min(ring[,1])-.012,min(ring[,2])-.012,max(ring[,1])+.012,max(ring[,2])+.012)
      m<-setView(m,p$attractions$longitude[1],p$attractions$latitude[1],zoom=17)
      htmlwidgets::onRender(m,.detail_page_assets$initializer)
    })
    detail_map_proxy <- function() {
      req(input$map_ready,context$attraction_id())
      leafletProxy("nearby_map",session=session)
    }
    # Only the facility layer depends on category/accessibility filters.
    observe({
      m<-clearGroup(detail_map_proxy(),"facilities");f<-facilities()
      for(kind in names(detail_types)){
        q<-f[f$facility_type==kind,,drop=FALSE];if(!nrow(q))next
        m<-addMarkers(m,lng=q$longitude,lat=q$latitude,layerId=paste0("facility:",q$facility_uid),group="facilities",label=paste(round(q$distance_m),"m"),
          icon=icons(iconUrl=vapply(q$facility_number,function(n)detail_facility_marker_url(kind,n),character(1)),iconWidth=36,iconHeight=34,iconAnchorX=18,iconAnchorY=17))
      }
    })
    observe({
      m<-clearGroup(detail_map_proxy(),"entrances")
      e<-p$detail$access_points;e<-e[e$attraction_id==context$attraction_id(),,drop=FALSE]
      if(nrow(e))addMarkers(m,lng=e$longitude,lat=e$latitude,group="entrances",layerId=e$access_point_uid,
        icon=icons(iconUrl="detail-icons/entrance.svg",iconWidth=24,iconHeight=24,iconAnchorX=12,iconAnchorY=12),
        label=vapply(seq_len(nrow(e)),function(i)detail_value(e$point_description[i]),character(1)))
    })
    observe({
      m<-clearGroup(detail_map_proxy(),"facility_selection");f<-facilities()
      q<-f[f$facility_uid%in%selected_facility(),,drop=FALSE]
      if(nrow(q))addCircleMarkers(m,lng=q$longitude,lat=q$latitude,radius=16,color="#54765b",weight=2,fillOpacity=.08,group="facility_selection",options=pathOptions(interactive=FALSE))
    })
    observe({
      m<-clearGroup(detail_map_proxy(),"attraction")
      s<-get_monthly_snapshot(p,context$month(),context$weather(),context$day_type(),context$hour())
      r<-s[s$attraction_id==context$attraction_id(),,drop=FALSE];req(nrow(r)==1)
      label<-htmltools::HTML(attraction_tooltip_html(p,r,context$month(),context$hour(),context$weather(),context$day_type()))
      addMarkers(m,lng=r$longitude,lat=r$latitude,group="attraction",layerId=paste0("attraction:",r$attraction_id),label=label,
        labelOptions=labelOptions(direction="auto",opacity=1),options=markerOptions(pane="detailAttraction"),
        icon=icons(iconUrl=r$icon_url,iconWidth=36,iconHeight=45,iconAnchorX=18,iconAnchorY=43))
    })
    # Filters and hour changes never reset the camera. Only radius/attraction or
    # an explicit facility/entrance selection changes the view.
    observeEvent(list(input$map_ready,context$attraction_id(),radius()), {
      m<-clearGroup(detail_map_proxy(),"radius")
      r<-p$attractions[p$attractions$attraction_id==context$attraction_id(),,drop=FALSE];req(nrow(r)==1)
      m<-addCircles(m,lng=r$longitude,lat=r$latitude,radius=radius(),color="#738a79",weight=1,dashArray="5 6",fillOpacity=.04,group="radius",options=pathOptions(interactive=FALSE))
      setView(m,r$longitude,r$latitude,zoom=if(radius()==300)17 else if(radius()==500)16.5 else 15.5)
    },priority=5)
    observeEvent(focus_request(), {
      m<-detail_map_proxy();f<-facilities()
      q<-f[f$facility_uid%in%selected_facility(),,drop=FALSE]
      if(nrow(q))setView(m,q$longitude,q$latitude,zoom=18)
      if(!is.null(focused_entry())){
        e<-p$detail$access_points;e<-e[e$attraction_id==context$attraction_id() & e$access_point_uid==focused_entry(),,drop=FALSE]
        if(nrow(e)==1)setView(m,e$longitude,e$latitude,zoom=18)
      }
    },ignoreInit=TRUE)
    observeEvent(input$entry_select,{
      e<-p$detail$access_points;e<-e[e$attraction_id==context$attraction_id() & e$access_point_uid==input$entry_select,,drop=FALSE]
      if(nrow(e)==1){selected_facility(NULL);focused_entry(input$entry_select);focus_request(focus_request()+1L);updateTabsetPanel(session,"info_tab",selected="Overview")}
    })
    output$overview <- renderUI({r<-row();req(nrow(r)==1)
      detail_section("Plan your visit",
        if(detail_has_value(r$description_en))tags$p(detail_value(r$description_en)),
        if(detail_has_value(r$highlights_en))tags$p(detail_value(r$highlights_en)),
        tags$dl(class="detail-facts",detail_fact("Visit scope",r$visit_scope,optional=TRUE),
          detail_fact("Opening hours",detail_visit_field(r$attraction_id,"opening_hours",r$opening_hours)),
          detail_fact("Admission",detail_visit_field(r$attraction_id,"admission_notes_en",r$admission_notes_en)),
          detail_fact("Booking",detail_visit_field(r$attraction_id,"booking_notes_en",r$booking_notes_en))),
        detail_safe_link(r$official_url,"Attraction website"))
    })
    output$access <- renderUI({
      r<-row();req(nrow(r)==1)
      e<-p$detail$access_points;e<-e[e$attraction_id==context$attraction_id(),,drop=FALSE]
      mapped<-!is.na(e$point_kind)&startsWith(e$point_kind,"mapped_")
      entrances<-e[mapped,,drop=FALSE];references<-e[!mapped,,drop=FALSE]
      point_buttons<-function(points)tagList(lapply(seq_len(nrow(points)),function(i)
        tags$button(type="button",class="transport-reset detail-entry-button",onclick=detail_click(ns("entry_select"),points$access_point_uid[i]),detail_icon("entrance"),detail_value(points$point_description[i]))))
      tags$details(class="detail-access-summary",tags$summary("Entrances & accessibility"),
        div(class="detail-access-body",div(class="detail-section",
          if(detail_has_value(r$accessibility_notes_en))tags$p(detail_value(r$accessibility_notes_en))
          else tags$p(class="detail-note",detail_missing),
          if(nrow(entrances))tagList(tags$h3("Entrances"),point_buttons(entrances)),
          if(nrow(references))tagList(tags$h3("Map locations"),point_buttons(references)))))
    })
    output$transport <- renderUI({s<-detail_transport_groups(nearby_stops());routes<-p$transport$routes;links<-p$transport$links
      detail_section("Nearby transport",
        actionButton(ns("view_transport"),"Open transport map",class="transport-reset"),
        tags$p(class="detail-note",sprintf("%d stops and stations within %d m · nearest 20 shown",nrow(s),radius())),
        if(!nrow(s))tags$p(class="detail-empty",detail_missing) else tagList(lapply(seq_len(min(20,nrow(s))),function(i){r<-s[i,];rr<-routes[routes$route_uid%in%links$route_uid[links$display_id%in%s$member_display_ids[[i]]],,drop=FALSE]
          div(class="selected-context",tags$strong(detail_value(r$name)),tags$p(class="detail-note",paste(detail_value(r$mode),"·",round(r$distance_m),"m · Routes:",detail_value(paste(unique(na.omit(rr$badge)),collapse=", ")))))})),
        tags$p(class="detail-note","Select a line and direction in the Transport panel."),
        uiOutput(ns("station_access")),uiOutput(ns("station_pathways")))
    })
    output$station_access<-renderUI({
      s<-nearby_stops();raw<-p$detail$transport_stops;mapping<-p$transport$stop_mapping
      ids<-mapping$stop_uid[mapping$display_id%in%s$display_id];r<-raw[raw$stop_uid%in%ids,,drop=FALSE]
      field<-intersect(c("wheelchair_boarding_effective","wheelchair_boarding_raw","wheelchair_boarding"),names(r))
      if(!length(field)||!nrow(r))return(NULL)
      # GTFS 0 means no information. Suppress unknown optional records instead of listing empty values.
      r<-r[!is.na(r[[field[1]]]) & r[[field[1]]]%in%c("1","2"),,drop=FALSE]
      if(!nrow(r))return(NULL)
      label<-ifelse(r[[field[1]]]=="1","Wheelchair boarding available","No wheelchair boarding")
      entries<-unique(paste0(r$stop_name," · ",label))
      tags$details(tags$summary("Boarding accessibility"),tags$ul(class="detail-source-list",lapply(entries,tags$li)))
    })
    output$station_pathways<-renderUI({
      req(context$attraction_id());a<-p$attractions[p$attractions$attraction_id==context$attraction_id(),,drop=FALSE]
      distance<-function(lon,lat){rad<-pi/180;dx<-(lon-a$longitude)*rad;dy<-(lat-a$latitude)*rad;z<-sin(dy/2)^2+cos(a$latitude*rad)*cos(lat*rad)*sin(dx/2)^2;2*6371000*asin(pmin(1,sqrt(z)))}
      paths<-p$detail$pathways
      keep<-pmin(distance(paths$from_longitude,paths$from_latitude),distance(paths$to_longitude,paths$to_latitude))<=radius()
      paths<-paths[!is.na(keep)&keep,,drop=FALSE]
      if(!nrow(paths))return(NULL)
      tags$details(tags$summary(sprintf("Nearby station pathways · %d",nrow(paths))),
        tags$table(class="detail-table",tags$thead(tags$tr(tags$th("From / to"),tags$th("Type"),tags$th("Seconds"))),tags$tbody(lapply(seq_len(nrow(paths)),function(i)tags$tr(tags$td(paste(detail_value(paths$from_name[i]),detail_value(paths$to_name[i]),sep=" → ")),tags$td(detail_value(paths$pathway_type[i])),tags$td(detail_value(paths$traversal_time_seconds_raw[i])))))))
    })
    output$sources <- renderUI({
      r<-row();req(nrow(r)==1)
      ids<-unique(na.omit(unlist(r[,grep("source_id$",names(r)),drop=FALSE],use.names=FALSE)))
      stop_ids<-p$transport$stop_mapping$stop_uid[p$transport$stop_mapping$display_id%in%nearby_stops()$display_id]
      relevant_gtfs<-unique(p$detail$transport_stops$source_id[p$detail$transport_stops$stop_uid%in%stop_ids])
      src<-p$detail$sources;src<-src[src$source_id%in%c(ids,relevant_gtfs,"COM_PED_COUNTS_2025","COM_PED_WEBSITE_HOURLY_2024","COM_PED_OPEN_DATA_COORDINATES_2024","BV_PUBLIC_HOLIDAYS_2024","WEA2024","OSM_POI_20250101","OSM_ELEMENT_HISTORY","WEA01"),,drop=FALSE]
      selected<-profile();selected<-selected[selected$hour==context$hour(),,drop=FALSE]
      div(class="detail-section",
        tags$h3("Selected dates"),
        tags$dl(class="detail-facts",if(identical(input$profile_basis,"typical")) detail_fact("Dates in selected weekday profile",
          sum(p$calendar$date %in% week_dates() & p$calendar$weekday == weekday())) else
          detail_fact("Dates in main map selection",length(context$dates())),
          if(nrow(selected))detail_fact("Valid observations at selected profile hour",selected$n)),
        tags$h3("Sources"),
        if(!nrow(src))tags$p(class="detail-empty",detail_missing) else tags$ul(class="detail-source-list",lapply(seq_len(nrow(src)),function(i)
          tags$li(tags$strong(detail_value(src$name[i])),if(detail_has_value(src$data_date[i]))paste0(" · ",src$data_date[i]),
            " ",detail_safe_link(src$source_url[i],"Source")))))
    })
    weekday <- reactive({v<-suppressWarnings(as.integer(input$weekday));if(length(v)!=1||is.na(v)||!v%in%1:7)1L else v})
    week_dates <- reactive(detail_week_dates(p,context$month(),context$weather()))
    week_profile <- reactive(detail_week_profile(p,context$attraction_id(),week_dates()))
    profile <- reactive({if(!identical(input$profile_basis,"typical"))return(detail_hour_profile(p,context$attraction_id(),context$dates()))
      w<-week_profile();w<-w[w$weekday_iso==weekday(),,drop=FALSE]
      data.frame(hour=w$hourday,mean=w$mean_pedestrian_count,p25=w$p25_pedestrian_count,p75=w$p75_pedestrian_count,n=w$n_observed)})
    main_context_label <- reactive(paste(p$month_labels[[context$month()]],paste0(p$analysis_year_label," ·"),
      if(context$weather()=="good_weather")"Good-weather days" else "Bad-weather days","·",
      if(context$day_type()=="holiday")"weekends + public holidays" else "weekdays excluding public holidays"))
    output$visiting_time_title<-renderUI({
      h<-context$hour()
      tags$h2(if(length(h)==1L&&!is.na(h)&&h%in%0:23)sprintf("Visiting time · %02d:00",h) else "Choose a visiting time")
    })
    week_context_label <- reactive(paste(p$month_labels[[context$month()]],paste0(p$analysis_year_label," ·"),
      if(context$weather()=="good_weather")"Good-weather days" else "Bad-weather days"))
    output$profile_note<-renderUI(tags$p(class="detail-note",
      if(identical(input$profile_basis,"typical"))paste(week_context_label(),"·",c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[weekday()],"mean within selected dates")
      else paste(main_context_label(),"· main map selection")))
    output$profile_legend<-renderUI(div(class="detail-chart-legend",
      tags$span(tags$i(class="detail-line-key",`aria-hidden`="true"),"Line: mean foot traffic"),
      tags$span(tags$i(class="detail-band-key",`aria-hidden`="true"),"Shaded band: middle 50% of observations"),
      if (is.finite(attraction_count_threshold(p,context$attraction_id(),"moderate")))
        tags$span(tags$i(class="detail-threshold-key detail-threshold-key-moderate",`aria-hidden`="true"),paste("Moderate threshold:",round_crowd_count(attraction_count_threshold(p,context$attraction_id(),"moderate")))),
      tags$span(tags$i(class="detail-threshold-key",`aria-hidden`="true"),paste("Busy threshold:",round_crowd_count(attraction_count_threshold(p,context$attraction_id()))))))
    output$hour_profile<-renderUI(detail_line_svg(profile(),ns,context$hour(),attraction_count_threshold(p,context$attraction_id()),"Mean",attraction_count_threshold(p,context$attraction_id(),"moderate")))
    output$selected_hour<-renderUI({
      r<-profile();r<-r[r$hour==context$hour(),,drop=FALSE]
      if(!nrow(r)||!is.finite(r$mean))return(NULL)
      statistic<-"Mean"
      tags$p(class="detail-note",sprintf("%02d:00 · %s %.1f street passages/hour · middle 50%%: %.1f–%.1f",context$hour(),statistic,round_crowd_count(r$mean),round_crowd_count(r$p25),round_crowd_count(r$p75)))
    })
    output$week_profile<-renderUI(detail_week_svg(week_profile(),ns,
      if(identical(input$profile_basis,"typical"))weekday() else 0L,context$hour()))
    day_content<-reactive({
      h<-context$hour();id<-context$attraction_id()
      values<-lapply(c("weekdays","holiday"),function(day){v<-p$monthly_profiles[[profile_key(context$month(),context$weather(),day)]]$rows;v[v$attraction_id==id&v$hourday==h,,drop=FALSE]})
      if(!any(vapply(values,function(v)nrow(v)>0L&&is.finite(v$display_count),logical(1))))return(NULL)
      cards<-lapply(seq_along(values),function(i){v<-values[[i]]
        div(class="detail-metric transport-reset",tags$span(if(i==1L)"Weekdays" else "Holiday"),
          tags$strong(if(!nrow(v)||!is.finite(v$display_count))detail_missing else format_count(round_crowd_count(v$display_count))),tags$small("mean passages / hour"))})
      tagList(tags$h3(sprintf("Weekdays vs holidays · %02d:00",h)),div(class="detail-metrics",cards),
        tags$p(class="detail-note",paste(p$month_labels[[context$month()]],paste0(p$analysis_year_label," ·"),if(context$weather()=="good_weather")"Good-weather days." else "Bad-weather days.","Holiday includes weekends and public holidays.")))
    })
    weather_rows<-reactive({
      w<-p$detail$weather
      keep<-w$sensing_date%in%context$dates()&w$hourday==context$hour()&w$weather_join_status=="ok"
      w[!is.na(keep)&keep,,drop=FALSE]
    })
    weather_content<-reactive({
      w<-weather_rows()
      fields<-c(temperature_2m_c="Mean temperature",apparent_temperature_c="Mean apparent temperature",precipitation_mm="Mean precipitation",wind_speed_10m_kmh="Mean wind speed")
      units<-c("°C","°C","mm/hour","km/h")
      known<-vapply(names(fields),function(k)any(is.finite(w[[k]])),logical(1))
      if(!any(known))return(NULL)
      tagList(tags$h3(sprintf("Weather · %02d:00",context$hour())),
        tags$p(class="detail-note",paste(main_context_label(),"· main map selection")),
        tags$dl(class="detail-facts",lapply(which(known),function(i){x<-w[[names(fields)[i]]];x<-x[is.finite(x)];detail_fact(unname(fields[i]),sprintf("%.1f %s",mean(x),units[i]))})),
        tags$p(class="detail-note","Average weather for the selected dates."))
    })
    output$day_compare<-renderUI(day_content())
    output$weather_summary<-renderUI(weather_content())
    output$summary_cards<-renderUI({
      day<-day_content();weather<-weather_content();if(is.null(day)&&is.null(weather))return(NULL)
      div(class="detail-summary-grid",
        if(!is.null(day))tags$section(class="detail-summary-card",day),
        if(!is.null(weather))tags$section(class="detail-summary-card",weather))
    })
    list(facilities=facilities,profile=profile,nearby_stops=nearby_stops,selected_facility=selected_facility,nearest_essentials=nearest_essentials)
  })
}
