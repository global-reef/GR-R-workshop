
dirs <- c("data", "scripts", "slides", "outputs/stats", "outputs/figures")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
rm(dirs)
