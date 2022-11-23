get_me_all_lines <- function(day, file) {
  con <- file(system.file(paste0("day-",day), file, package = "ranswers"), "r")
  lines <- readLines(con)
  close(con)
  lines
}

