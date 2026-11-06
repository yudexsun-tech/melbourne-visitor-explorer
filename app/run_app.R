arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- arguments[grepl("^--file=", arguments)]
source_file <- if (length(file_argument)) {
  sub("^--file=", "", file_argument[1])
} else {
  frame_files <- lapply(sys.frames(), function(frame) frame$ofile)
  frame_files <- Filter(function(x) is.character(x) && length(x) == 1L, frame_files)
  if (length(frame_files)) tail(frame_files, 1)[[1]] else file.path(getwd(), "run_app.R")
}
app_dir <- dirname(normalizePath(source_file, mustWork = TRUE))
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop('请先安装 README.md 列出的 R 依赖；主地图需要 Mapbox 公开令牌与网络，其他地图保留本地数据。')
}
launch_viewer <- if (interactive()) getOption("viewer", default = utils::browseURL) else FALSE
shiny::runApp(app_dir, host = "127.0.0.1", port = 3840, launch.browser = launch_viewer)
