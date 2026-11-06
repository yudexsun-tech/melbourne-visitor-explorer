# Derived cache only; raw files remain authoritative. Content hashes survive ZIP extraction.
load_cached_project <- function(build, cache_file = file.path("cache", "project.rds")) {
  paths <- sort(c(list.files("data", recursive=TRUE, full.names=TRUE),
    list.files("R", pattern="\\.R$", full.names=TRUE),
    list.files("metadata", recursive=TRUE, full.names=TRUE), "app.R"))
  paths <- paths[!file.info(paths)$isdir]
  signature <- tools::md5sum(paths)
  if (file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error=function(e) NULL)
    if (is.list(cached) && identical(cached$signature, signature) &&
        is.list(cached$project)) return(cached$project)
  }
  project <- build()
  # Read-only installations still run normally without writing a cache.
  tryCatch({
    dir.create(dirname(cache_file), recursive=TRUE, showWarnings=FALSE)
    temporary <- tempfile("project-", tmpdir=dirname(cache_file))
    on.exit(unlink(temporary), add=TRUE)
    saveRDS(list(signature=signature, project=project), temporary, compress=FALSE)
    file.rename(temporary, cache_file)
  }, error=function(e) message("Project cache unavailable; using loaded data."))
  project
}
