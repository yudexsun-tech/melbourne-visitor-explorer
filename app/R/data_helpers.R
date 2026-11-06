read_selected_csv <- function(path, column_types) {
  header <- names(read.csv(path, nrows = 0, check.names = FALSE,
                           fileEncoding = "UTF-8-BOM"))
  missing_columns <- setdiff(names(column_types), header)
  if (length(missing_columns)) {
    stop("数据文件缺少字段：", paste(missing_columns, collapse = ", "))
  }
  classes <- setNames(rep("NULL", length(header)), header)
  classes[names(column_types)] <- "character"
  result <- read.csv(path, colClasses = classes, check.names = FALSE,
                     fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                     na.strings = c("", "NA"))
  for (field in names(column_types)) {
    if (column_types[[field]] %in% c("integer", "numeric")) {
      converted <- suppressWarnings(as.numeric(result[[field]]))
      if (any(!is.na(result[[field]]) & is.na(converted))) stop("无法转换数值字段：", field)
      if (column_types[[field]] == "integer") {
        if (any(converted != floor(converted), na.rm = TRUE)) stop("整数列包含小数：", field)
        converted <- as.integer(converted)
      }
      result[[field]] <- converted
    }
  }
  result
}

load_map_data <- function(data_dir) {
  attractions <- read_selected_csv(file.path(data_dir, "attractions_2025.csv"), c(
    attraction_id = "character", name_en = "character", setting = "character",
    latitude = "numeric", longitude = "numeric"
  ))
  conditions <- read_selected_csv(
    file.path(data_dir, "attraction_conditions_hourly_2025.csv"), c(
      attraction_id = "character", location_id = "character",
      sensing_date = "character", hourday = "integer",
      pedestriancount = "numeric", crowd_class = "character",
      crowd_class_status = "character", sensor_distance_m = "numeric",
      shared_sensor_attraction_count = "integer"
    )
  )

  display <- data.frame(
    attraction_id = c(sprintf("ATTR%03d", 1:12), "ATTR016"),
    name_zh = c("维多利亚州立图书馆", "NGV 澳大利亚馆", "ACMI 影像博物馆",
                "移民博物馆", "圣保罗大教堂", "维多利亚女王市场",
                "皇家拱廊", "布洛克拱廊", "伯克街购物中心", "唐人街",
                "弗林德斯街车站外观", "联邦广场公共广场", "霍西尔巷"),
    icon_key = c("book", "art", "film", "globe", "church", "basket",
                 "crown", "arch", "bag", "lantern", "train", "square", "brush"),
    number = sprintf("%02d", 1:13), stringsAsFactors = FALSE
  )
  position <- match(attractions$attraction_id, display$attraction_id)
  if (anyNA(position)) stop("请在 R/data_helpers.R 的 display 中补充新景点的显示配置。")
  attractions$name_zh <- display$name_zh[position]
  attractions$icon_key <- display$icon_key[position]
  attractions$number <- display$number[position]
  attractions <- attractions[order(attractions$number), ]
  rownames(attractions) <- NULL

  key <- paste(conditions$attraction_id, conditions$sensing_date, conditions$hourday)
  if (anyDuplicated(key)) stop("小时数据存在重复的景点＋日期＋小时，无法安全展示。")
  if (anyDuplicated(attractions$attraction_id)) stop("景点主表 ID 重复。")
  if (anyNA(attractions$latitude) || anyNA(attractions$longitude)) stop("景点坐标缺失。")
  if (anyNA(conditions$hourday) || any(!conditions$hourday %in% 0:23)) stop("小时必须为 0–23。")
  if (!all(conditions$attraction_id %in% attractions$attraction_id)) stop("小时数据存在未知景点。")

  dates <- sort(unique(conditions$sensing_date))
  list(
    attractions = attractions, conditions = conditions, dates = dates,
    date_index = split(seq_len(nrow(conditions)), conditions$sensing_date),
    boundary = jsonlite::fromJSON(file.path(data_dir, "ftz_boundary_2025.geojson"),
                                  simplifyVector = FALSE)
  )
}

get_hour_snapshot <- function(project, date, hour, apply_hours = TRUE) {
  if (length(hour) != 1 || is.na(hour) || !hour %in% 0:23) stop("小时必须为 0–23。")
  if (length(date) != 1 || !date %in% project$dates) stop("所选日期不在数据范围内。")
  rows <- project$conditions[project$date_index[[as.character(date)]], , drop = FALSE]
  rows <- rows[rows$hourday == hour, , drop = FALSE]
  result <- project$attractions
  matched <- match(result$attraction_id, rows$attraction_id)
  for (field in setdiff(names(rows), "attraction_id")) result[[field]] <- rows[[field]][matched]
  valid <- !is.na(result$crowd_class_status) & result$crowd_class_status == "classified" &
    !is.na(result$pedestriancount) & result$crowd_class %in% c("Quiet", "Moderate", "Crowded")
  result$display_count <- ifelse(valid, result$pedestriancount, NA_real_)
  result$display_class <- ifelse(valid, result$crowd_class, "Unavailable")
  result$color_key <- unname(c(Quiet = "quiet", Moderate = "moderate", Crowded = "crowded", Unavailable = "unknown")[result$display_class])
  result$status_zh <- unname(c(Quiet = "Quiet", Moderate = "Moderate", Crowded = "Busy", Unavailable = "")[result$display_class])
  result$icon_url <- sprintf("icons/%s-%s.svg", result$icon_key, result$color_key)
  if (isTRUE(apply_hours)) result <- apply_attraction_opening_status(project, result, date, hour)
  result
}

format_count <- function(value) {
  ifelse(is.na(value), "—", format(value, big.mark = ",", scientific = FALSE, trim = TRUE))
}

# One rounding rule for map labels, lists, detail summaries and chart tooltips.
round_crowd_count <- function(value) round(value, 1)

clock_svg <- function(hour) {
  ticks <- paste(vapply(0:11, function(i) sprintf(
    '<line x1="40" y1="7" x2="40" y2="%s" transform="rotate(%s 40 40)"/>',
    if (i %% 3 == 0) 13 else 10, i * 30
  ), character(1)), collapse = "")
  htmltools::HTML(sprintf(
    paste0('<svg class="analog-clock" viewBox="0 0 80 80" aria-hidden="true">',
           '<circle cx="40" cy="40" r="38" fill="#f1f5ef" stroke="#d8e1d6"/>',
           '<g stroke="#8a9a8a" stroke-width="1.4">%s</g>',
           '<line x1="40" y1="40" x2="40" y2="20" stroke="#203a32" stroke-width="4" ',
           'stroke-linecap="round" transform="rotate(%s 40 40)"/>',
           '<line x1="40" y1="40" x2="40" y2="12" stroke="#6d8e78" stroke-width="2.5"/>',
           '<circle cx="40" cy="40" r="3" fill="#203a32"/></svg>'), ticks, (hour %% 12) * 30
  ))
}
