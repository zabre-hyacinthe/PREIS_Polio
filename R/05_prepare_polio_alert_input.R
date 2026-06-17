# ============================================================
# R/05_prepare_polio_alert_input.R — PREIS POLIO FV
# Build fresh alert/email/dashboard inputs from latest GPEI page
# DEFINITIVE FIX: reporting date cannot be future
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(rvest)
  library(xml2)
  library(lubridate)
})

.clean_txt <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[is.na(x)] <- ""
  x
}

norm_country_for_join <- function(x) {
  x <- .clean_txt(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[’`]", "'")
  x <- stringr::str_replace_all(x, "&", "and")
  x <- stringr::str_replace_all(x, "[^a-z0-9 ]", " ")
  x <- stringr::str_squish(x)
  
  dplyr::case_when(
    x %in% c("dr congo", "drc", "democratic republic of congo",
             "democratic republic of the congo") ~ "democratic republic of the congo",
    x %in% c("cote d ivoire", "cote divoire", "ivory coast") ~ "cote d ivoire",
    x %in% c("tanzania", "united republic of tanzania") ~ "tanzania",
    x %in% c("cape verde", "cabo verde") ~ "cape verde",
    TRUE ~ x
  )
}

parse_all_dates_non_future <- function(x, extraction_date = Sys.Date()) {
  
  x <- paste(x, collapse = " ")
  x <- stringr::str_squish(x)
  
  patterns <- c(
    "\\b\\d{1,2}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)\\s+20[0-9]{2}\\b",
    "\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s*20[0-9]{2}\\b",
    "\\b20[0-9]{2}-\\d{2}-\\d{2}\\b"
  )
  
  candidates <- unlist(lapply(patterns, function(p) {
    stringr::str_extract_all(x, stringr::regex(p, ignore_case = TRUE))[[1]]
  }))
  
  candidates <- unique(na.omit(candidates))
  
  if (length(candidates) == 0) {
    return(as.Date(NA))
  }
  
  parsed <- suppressWarnings(lubridate::parse_date_time(
    candidates,
    orders = c("d B Y", "B d Y", "Y-m-d"),
    locale = "C"
  ))
  
  parsed <- as.Date(parsed)
  parsed <- parsed[!is.na(parsed)]
  
  if (length(parsed) == 0) {
    return(as.Date(NA))
  }
  
  extraction_date <- as.Date(extraction_date)
  
  # CRITICAL FIX: remove every future date
  parsed <- parsed[parsed <= extraction_date]
  
  if (length(parsed) == 0) {
    return(as.Date(NA))
  }
  
  max(parsed)
}

format_reporting_date_label <- function(x) {
  x <- as.Date(x)
  if (is.na(x)) return(NA_character_)
  format(x, "%d %B %Y")
}

make_issue_id <- function(reporting_date) {
  reporting_date <- as.Date(reporting_date)
  paste0(
    "polio_this_week__",
    format(reporting_date, "%d_%B_%Y") |>
      stringr::str_to_lower()
  )
}

detect_virus_type <- function(x) {
  x <- tolower(.clean_txt(x))
  dplyr::case_when(
    stringr::str_detect(x, "cvdpv2") ~ "cVDPV2",
    stringr::str_detect(x, "cvdpv1") ~ "cVDPV1",
    stringr::str_detect(x, "cvdpv3") ~ "cVDPV3",
    stringr::str_detect(x, "wpv1") ~ "WPV1",
    stringr::str_detect(x, "wpv") ~ "WPV",
    TRUE ~ "Unspecified"
  )
}

detect_signal_type <- function(x) {
  x <- tolower(.clean_txt(x))
  dplyr::case_when(
    stringr::str_detect(x, "environmental") & stringr::str_detect(x, "case") ~ "Case + Environmental",
    stringr::str_detect(x, "environmental|sample") ~ "Environmental",
    stringr::str_detect(x, "case|cases") ~ "Case",
    TRUE ~ "Unspecified"
  )
}

detect_count <- function(x) {
  x0 <- tolower(.clean_txt(x))
  
  word_map <- c(
    "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
    "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9, "ten" = 10
  )
  
  num <- stringr::str_extract(x0, "\\b[0-9]+\\b")
  if (!is.na(num)) return(as.integer(num))
  
  for (w in names(word_map)) {
    if (stringr::str_detect(x0, paste0("\\b", w, "\\b"))) {
      return(as.integer(word_map[[w]]))
    }
  }
  
  return(1L)
}

extract_gpei_page <- function(url) {
  page <- xml2::read_html(url)
  txt <- page |>
    rvest::html_text2() |>
    stringr::str_squish()
  
  list(page = page, text = txt)
}

extract_weekly_summary_bullets_from_page <- function(page) {
  
  bullets <- page |>
    rvest::html_elements("li") |>
    rvest::html_text2() |>
    stringr::str_squish()
  
  bullets <- bullets[
    stringr::str_detect(
      bullets,
      stringr::regex("cVDPV|WPV|environmental|case|cases|positive", ignore_case = TRUE)
    )
  ]
  
  bullets <- bullets[stringr::str_detect(bullets, ":")]
  bullets <- unique(bullets)
  
  tibble::tibble(raw_bullet = bullets)
}

.prepare_rcc_reference <- function(rcc_file) {
  
  ref <- readr::read_csv(rcc_file, show_col_types = FALSE)
  
  names(ref) <- names(ref) |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("_$", "")
  
  country_col <- dplyr::case_when(
    "country" %in% names(ref) ~ "country",
    "country_name" %in% names(ref) ~ "country_name",
    "geo_name_short" %in% names(ref) ~ "geo_name_short",
    TRUE ~ NA_character_
  )
  
  if (is.na(country_col)) {
    stop("Country column not found in RCC file.")
  }
  
  if (!"rcc" %in% names(ref)) {
    stop("RCC column not found in RCC file.")
  }
  
  if (!"iso3" %in% names(ref)) {
    ref$iso3 <- NA_character_
  }
  
  ref |>
    transmute(
      country_join = norm_country_for_join(.data[[country_col]]),
      iso3 = .clean_txt(iso3),
      rcc = .clean_txt(rcc)
    ) |>
    distinct(country_join, .keep_all = TRUE)
}

.report_unmapped_countries <- function(df) {
  unmapped <- df |>
    filter(is.na(rcc) | rcc == "") |>
    distinct(country_raw)
  
  if (nrow(unmapped) > 0) {
    warning(
      "[POLIO PREP] Unmapped countries: ",
      paste(unmapped$country_raw, collapse = ", "),
      call. = FALSE
    )
  }
}

prepare_polio_alert_input <- function(
    url = "https://polioeradication.org/about-polio/polio-this-week/",
    rcc_file = "config/rcc_country_fv.csv",
    out_file_main = "dashboard/data/dashboard/polio_africa_email_input.csv",
    out_file_alt  = "dashboard/data/dashboard/polio_alert_input.csv",
    out_last_issue = "dashboard/data/dashboard/polio_last_issue.csv",
    project_dir = NULL,
    latest_issue = NULL,
    force_refresh = TRUE
) {
  
  root_dir <- if (!is.null(project_dir) && nzchar(project_dir)) {
    project_dir
  } else if (exists("ROOT", envir = .GlobalEnv)) {
    get("ROOT", envir = .GlobalEnv)
  } else {
    getwd()
  }
  
  root_dir <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
  
  resolve_project_path <- function(fp, must_exist = TRUE) {
    if (grepl("^[A-Za-z]:/|^/", fp)) {
      out <- normalizePath(fp, winslash = "/", mustWork = FALSE)
    } else {
      out <- normalizePath(file.path(root_dir, fp), winslash = "/", mustWork = FALSE)
    }
    
    if (must_exist && !file.exists(out)) {
      stop("File not found: ", out, call. = FALSE)
    }
    
    out
  }
  
  rcc_file       <- resolve_project_path(rcc_file, TRUE)
  out_file_main  <- resolve_project_path(out_file_main, FALSE)
  out_file_alt   <- resolve_project_path(out_file_alt, FALSE)
  out_last_issue <- resolve_project_path(out_last_issue, FALSE)
  
  extraction_datetime <- Sys.time()
  extraction_date <- as.Date(extraction_datetime)
  
  gpei <- extract_gpei_page(url)
  
  reporting_date <- parse_all_dates_non_future(
    x = gpei$text,
    extraction_date = extraction_date
  )
  
  # If the website date extraction fails, use extraction date.
  # Never use latest_issue if it contains a future/wrong date.
  if (is.na(reporting_date)) {
    reporting_date <- extraction_date
    warning(
      "[POLIO PREP] No valid non-future GPEI date found. ",
      "Using extraction date: ",
      reporting_date,
      call. = FALSE
    )
  }
  
  if (reporting_date > extraction_date) {
    stop(
      "[POLIO PREP] Critical error: future reporting date detected: ",
      reporting_date,
      ". Pipeline stopped.",
      call. = FALSE
    )
  }
  
  reporting_date_label <- format_reporting_date_label(reporting_date)
  issue_id <- make_issue_id(reporting_date)
  
  message("[POLIO PREP] Reporting date fixed to: ", reporting_date_label)
  message("[POLIO PREP] Issue ID fixed to: ", issue_id)
  
  bullets <- extract_weekly_summary_bullets_from_page(gpei$page)
  
  if (nrow(bullets) == 0) {
    stop("No weekly summary bullets extracted from GPEI page.", call. = FALSE)
  }
  
  df <- bullets |>
    mutate(
      country_raw = str_trim(str_extract(raw_bullet, "^[^:]+")),
      summary_text = str_trim(str_replace(raw_bullet, "^[^:]+\\:", "")),
      
      country_raw = case_when(
        str_detect(country_raw, regex("^Dr\\.?\\s*Congo|^DRC$|Democratic Republic Of Congo", ignore_case = TRUE)) ~ "Democratic Republic of the Congo",
        str_detect(country_raw, regex("^Republic of (the )?Congo", ignore_case = TRUE)) ~ "Congo Republic",
        str_detect(country_raw, regex("C(o|ô)te\\s+d.?Ivoire|Ivory Coast", ignore_case = TRUE)) ~ "Cote d'Ivoire",
        str_detect(country_raw, regex("^Cabo Verde$", ignore_case = TRUE)) ~ "Cape Verde",
        str_detect(country_raw, regex("S(a|ã)o Tom(e|é)", ignore_case = TRUE)) ~ "Sao Tome and Principe",
        str_detect(country_raw, regex("^Comoro", ignore_case = TRUE)) ~ "Comoros",
        str_detect(country_raw, regex("Guinea.Bissau|Guinea Bissau", ignore_case = TRUE)) ~ "Guinea-Bissau",
        str_detect(country_raw, regex("^Swaziland$", ignore_case = TRUE)) ~ "Eswatini",
        str_detect(country_raw, regex("United Republic of Tanzania|Tanzania,", ignore_case = TRUE)) ~ "Tanzania",
        TRUE ~ country_raw
      ),
      
      country = norm_country_for_join(country_raw),
      
      # DEFINITIVE DATE FIELDS
      issue_id = issue_id,
      issue_date = as.character(reporting_date),
      report_date = reporting_date_label,
      reporting_date = as.character(reporting_date),
      reporting_date_label = reporting_date_label,
      
      source_url = url,
      fetched_date = as.character(extraction_date),
      extracted_at = format(extraction_datetime, "%Y-%m-%d %H:%M:%S %Z"),
      
      virus_type = vapply(summary_text, detect_virus_type, character(1)),
      signal_type = vapply(summary_text, detect_signal_type, character(1)),
      event_type = case_when(
        str_detect(tolower(signal_type), "environment") & str_detect(tolower(signal_type), "case") ~ "Case + Environmental",
        str_detect(tolower(signal_type), "environment") ~ "Environmental",
        str_detect(tolower(signal_type), "case") ~ "Case",
        TRUE ~ "Unspecified"
      ),
      count = vapply(summary_text, detect_count, integer(1)),
      location_text = NA_character_,
      onset_date = NA_character_,
      pathogen = "Polio virus",
      geo_level = "country"
    )
  
  ref2 <- .prepare_rcc_reference(rcc_file)
  
  df_joined <- df |>
    left_join(ref2, by = c("country" = "country_join"))
  
  .report_unmapped_countries(df_joined)
  
  out <- df_joined |>
    mutate(
      iso3 = .clean_txt(iso3),
      rcc = .clean_txt(rcc),
      is_africa = !is.na(rcc) & nzchar(rcc)
    ) |>
    filter(is_africa) |>
    select(
      issue_id,
      issue_date,
      report_date,
      reporting_date,
      reporting_date_label,
      country_raw,
      country,
      iso3,
      rcc,
      virus_type,
      signal_type,
      event_type,
      count,
      summary_text,
      source_url,
      fetched_date,
      extracted_at,
      location_text,
      onset_date,
      pathogen,
      geo_level,
      raw_bullet
    )
  
  if (nrow(out) == 0) {
    stop("No African polio rows retained after RCC mapping.", call. = FALSE)
  }
  
  # FINAL HARD CHECK
  if (any(as.Date(out$reporting_date) > Sys.Date(), na.rm = TRUE)) {
    stop("Critical error: future reporting_date detected in output.", call. = FALSE)
  }
  
  if (any(grepl("20 June 2026", out$report_date, ignore.case = TRUE))) {
    stop("Critical error: wrong report_date '20 June 2026' still present.", call. = FALSE)
  }
  
  dir.create(dirname(out_file_main), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_file_alt), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_last_issue), recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(out, out_file_main)
  readr::write_csv(out, out_file_alt)
  
  readr::write_csv(
    tibble(
      issue_id = issue_id,
      issue_date = as.character(reporting_date),
      report_date = reporting_date_label,
      reporting_date = as.character(reporting_date),
      reporting_date_label = reporting_date_label,
      source_url = url,
      extracted_at = format(extraction_datetime, "%Y-%m-%d %H:%M:%S %Z")
    ),
    out_last_issue
  )
  
  message("[POLIO PREP] Saved: ", out_file_main)
  message("[POLIO PREP] Saved: ", out_file_alt)
  message("[POLIO PREP] Saved: ", out_last_issue)
  message("[POLIO PREP] FINAL reporting_date: ", reporting_date_label)
  message("[POLIO PREP] African polio rows retained: ", nrow(out))
  message("[POLIO PREP] Affected African countries retained: ", n_distinct(out$country))
  
  invisible(out)
}