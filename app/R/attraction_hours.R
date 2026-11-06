# Expand calendar abbreviations only in opening-hour text, never names or IDs.
detail_opening_hours_text <- function(value) {
  if (!length(value) || is.na(value[1])) return(value)
  value <- as.character(value)
  if (identical(value, "24/7")) return("Open 24 hours every day.")
  days <- list(Monday = c("Mo", "Mon"), Tuesday = c("Tu", "Tue", "Tues"),
    Wednesday = c("We", "Wed"), Thursday = c("Th", "Thu", "Thur", "Thurs"),
    Friday = c("Fr", "Fri"), Saturday = c("Sa", "Sat"), Sunday = c("Su", "Sun"))
  for (day in names(days)) value <- gsub(paste0("\\b(", paste(days[[day]], collapse = "|"), ")\\b"),
    day, value, perl = TRUE)
  all_days <- paste(names(days), collapse = "|")
  value <- gsub(paste0("(", all_days, ")\\s*[-–]\\s*(", all_days, ")"),
    "\\1 to \\2", value, perl = TRUE)
  value <- gsub(",(?=[A-Za-z])", ", ", value, perl = TRUE)
  value
}

load_attraction_opening_hours <- function(project, data_dir) {
  weekly <- read_selected_csv(file.path(data_dir, "attraction_opening_hours.csv"), c(
    attraction_id = "character", weekday_iso = "integer", status = "character",
    opens_minute = "numeric", closes_minute = "numeric"))
  exceptions <- read_selected_csv(file.path(data_dir, "attraction_opening_exceptions.csv"), c(
    attraction_id = "character", date = "character", status = "character",
    opens_minute = "numeric", closes_minute = "numeric"))
  stopifnot(nrow(weekly) == nrow(project$attractions) * 7L,
    setequal(weekly$attraction_id, project$attractions$attraction_id),
    all(weekly$weekday_iso %in% 1:7),
    !anyDuplicated(paste(weekly$attraction_id, weekly$weekday_iso)),
    !anyDuplicated(paste(exceptions$attraction_id, exceptions$date)),
    all(exceptions$attraction_id %in% project$attractions$attraction_id))
  for (rows in list(weekly, exceptions)) {
    open <- rows$status == "open"
    stopifnot(all(rows$status %in% c("open", "closed", "unknown")),
      all(is.finite(rows$opens_minute[open]) & is.finite(rows$closes_minute[open])),
      all(rows$opens_minute[open] >= 0 & rows$closes_minute[open] <= 1440),
      all(rows$opens_minute[open] < rows$closes_minute[open]))
  }
  project$opening_hours <- list(weekly = weekly, exceptions = exceptions)
  project
}

# TRUE/FALSE/NA correspond to known open, known closed and unspecified hours.
attraction_open_on_dates <- function(project, id, dates, hour) {
  dates <- as.character(dates)
  weekday <- (as.integer(as.Date(dates)) + 3L) %% 7L + 1L
  weekly <- project$opening_hours$weekly
  rows <- weekly[match(paste(id, weekday), paste(weekly$attraction_id, weekly$weekday_iso)), ]
  exceptions <- project$opening_hours$exceptions
  index <- match(paste(id, dates), paste(exceptions$attraction_id, exceptions$date))
  found <- which(!is.na(index))
  for (field in c("status", "opens_minute", "closes_minute"))
    rows[[field]][found] <- exceptions[[field]][index[found]]
  result <- rep(NA, length(dates))
  result[which(rows$status == "closed")] <- FALSE
  open <- which(rows$status == "open")
  result[open] <- hour * 60 >= rows$opens_minute[open] & hour * 60 < rows$closes_minute[open]
  result
}

attraction_opening_state <- function(project, id, dates, hour) {
  open <- attraction_open_on_dates(project, id, unique(as.character(dates)), hour)
  if (!length(open)) return("unknown")
  if (all(open %in% TRUE)) return("open")
  if (any(open %in% TRUE)) return("mixed")
  if (anyNA(open)) return("unknown")
  "closed"
}

apply_attraction_opening_status <- function(project, rows, dates, hour) {
  if (is.null(project$opening_hours)) return(rows)
  rows$crowd_color_key <- rows$color_key
  rows$crowd_status <- rows$status_zh
  dates <- unique(as.character(dates))
  rows$opening_status <- vapply(rows$attraction_id, function(id) {
    attraction_opening_state(project, id, dates, hour)
  }, character(1), USE.NAMES = FALSE)
  closed <- rows$opening_status == "closed"
  unknown <- rows$opening_status == "unknown"
  mixed <- rows$opening_status == "mixed"
  rows$color_key[closed | unknown] <- "unknown"
  rows$status_zh[closed] <- "Closed"
  rows$status_zh[unknown] <- "Not available"
  rows$status_zh[mixed] <- paste(rows$status_zh[mixed], "Hours vary by day", sep = " · ")
  rows$icon_url <- sprintf("icons/%s-%s.svg", rows$icon_key, rows$color_key)
  rows
}

# With no sampled dates for a weekday, keep its regular hours separate from data availability.
attraction_weekday_opening_state <- function(project, id, weekday, hour) {
  rows <- project$opening_hours$weekly
  row <- rows[rows$attraction_id == id & rows$weekday_iso == weekday, , drop = FALSE]
  if (nrow(row) != 1L || row$status == "unknown") return("unknown")
  if (row$status == "closed") return("closed")
  if (hour * 60 >= row$opens_minute && hour * 60 < row$closes_minute) "open" else "closed"
}
