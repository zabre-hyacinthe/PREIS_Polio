# =========================================================
# R/02_check_polio_update.R
# Vérification robuste des mises à jour Polio
# =========================================================

suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(stringr)
})

source("R/03_issue_registry.R")

normalize_issue_id <- function(issue_date) {
  clean_date <- issue_date |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")
  
  paste0("polio_this_week__", clean_date)
}

extract_current_issue_direct <- function(
    url = "https://polioeradication.org/about-polio/polio-this-week/"
) {
  page <- read_html(url)
  page_text <- html_text2(page)
  
  m <- stringr::str_match(
    page_text,
    "(?i)Country\\s+updates\\s+as\\s+of\\s+([0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4})"
  )
  
  issue_date <- m[, 2]
  
  if (length(issue_date) == 0 || is.na(issue_date) || !nzchar(issue_date)) {
    stop("Unable to extract current polio issue from source page.")
  }
  
  issue_id <- normalize_issue_id(issue_date)
  
  list(
    issue_date = issue_date,
    issue_id   = issue_id,
    source_url = url
  )
}

check_polio_update <- function(root = ".") {
  current_info <- extract_current_issue_direct()
  last_issue <- read_last_issue(root)
  
  is_new <- is.na(last_issue) || !identical(current_info$issue_id, last_issue)
  
  if (is_new) {
    message("NEW POLIO UPDATE DETECTED")
    message("Previous: ", ifelse(is.na(last_issue), "NA", last_issue))
    message("Current : ", current_info$issue_id)
  } else {
    message("NO NEW POLIO UPDATE")
    message("Current issue unchanged: ", current_info$issue_id)
  }
  
  list(
    is_new = is_new,
    current_issue = current_info$issue_id,
    current_date = current_info$issue_date,
    last_issue = last_issue,
    source_url = current_info$source_url
  )
}