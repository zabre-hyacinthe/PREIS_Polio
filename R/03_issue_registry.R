
# R/03_issue_registry.R — Gestion du registre last_issue

get_issue_registry_path <- function(root = ".") {
  file.path(root, "data", "last_issue.txt")
}

ensure_issue_registry_dir <- function(root = ".") {
  dir.create(file.path(root, "data"), recursive = TRUE, showWarnings = FALSE)
}

read_last_issue <- function(root = ".") {
  path <- get_issue_registry_path(root)
  if (!file.exists(path)) return(NA_character_)
  x <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  x <- x[nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

write_last_issue <- function(issue_id, root = ".") {
  ensure_issue_registry_dir(root)
  path <- get_issue_registry_path(root)
  writeLines(issue_id, path, useBytes = TRUE)
  invisible(path)
}

