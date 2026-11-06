# =========================================================
# R/02_check_polio_update.R  (v2 - 2026-11-06)
# Verification robuste des mises a jour Polio + garde-fou "edition plus ancienne"
# v2 : une edition dont la date "as of" est ANTERIEURE a la derniere edition traitee
#      n'est plus consideree comme nouvelle (is_new = FALSE, is_stale = TRUE).
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

# Analyse de date independante de la langue du systeme (pas de %B)
parse_issue_date <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return(as.Date(NA))
  m <- stringr::str_match(x[1], "^([0-9]{1,2})[ _]+([a-z]+)[ _]+([0-9]{4})$")
  if (is.na(m[1, 1])) return(as.Date(NA))
  months <- c(january = 1, february = 2, march = 3, april = 4, may = 5, june = 6,
              july = 7, august = 8, september = 9, october = 10, november = 11, december = 12)
  mo <- unname(months[m[1, 3]])
  if (is.na(mo)) return(as.Date(NA))
  tryCatch(
    as.Date(sprintf("%04d-%02d-%02d", as.integer(m[1, 4]), as.integer(mo), as.integer(m[1, 2]))),
    error = function(e) as.Date(NA)
  )
}

issue_id_to_date <- function(issue_id) {
  if (is.null(issue_id) || length(issue_id) == 0 || is.na(issue_id[1])) return(as.Date(NA))
  txt <- sub("^polio_this_week__?", "", as.character(issue_id[1]))
  parse_issue_date(gsub("_", " ", txt))
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

  differs <- is.na(last_issue) || !identical(current_info$issue_id, last_issue)

  cur_date  <- parse_issue_date(current_info$issue_date)
  last_date <- issue_id_to_date(last_issue)

  # Edition plus ANCIENNE que la derniere traitee : source qui recule (cache,
  # republication d'une ancienne edition...). Ne jamais la traiter comme nouvelle.
  is_stale <- isTRUE(differs) && !is.na(cur_date) && !is.na(last_date) && cur_date < last_date
  is_new   <- isTRUE(differs) && !is_stale

  if (is_stale) {
    message("STALE POLIO SOURCE: page date (", format(cur_date), ") is OLDER than the last processed edition (", format(last_date), ")")
    message("Previous: ", last_issue)
    message("Current : ", current_info$issue_id)
  } else if (is_new) {
    message("NEW POLIO UPDATE DETECTED")
    message("Previous: ", ifelse(is.na(last_issue), "NA", last_issue))
    message("Current : ", current_info$issue_id)
    if (is.na(cur_date)) message("WARNING: issue date could not be parsed; recency check skipped.")
  } else {
    message("NO NEW POLIO UPDATE")
    message("Current issue unchanged: ", current_info$issue_id)
  }

  list(
    is_new = is_new,
    is_stale = is_stale,
    current_issue = current_info$issue_id,
    current_date = current_info$issue_date,
    last_issue = last_issue,
    source_url = current_info$source_url
  )
}