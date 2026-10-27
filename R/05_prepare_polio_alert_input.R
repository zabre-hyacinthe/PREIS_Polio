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

# ------------------------------------------------------------
# COMPTAGE ATOMIQUE (ajoute le 27 oct. 2026)
#
# detect_count() ci-dessus prenait le PREMIER nombre du texte et valait 1 par
# defaut : "one cVDPV2 case and three cVDPV2 positive environmental samples"
# donnait 1 au lieu de 4, et un texte sans nombre donnait 1. Elle n'est plus
# utilisee pour la colonne count (conservee uniquement pour compatibilite).
#
# Le comptage s'appuie maintenant sur R/lib_atomic_parser.R
# (segment_atomic_events), verifie par tests/test_atomic_audit.R. Regles :
#   - un texte dont l'extraction est ambigue, vide, ou qui couvre plusieurs
#     pays donne count = NA et count_status = "review" : jamais 1 par defaut,
#     jamais de valeur inventee ;
#   - le signal n'est PAS masque : la ligne est conservee, son type de virus
#     et son type d'evenement viennent alors des mots-cles du texte (tous les
#     virus mentionnes, pour ne jamais sous-classer un WPV) ;
#   - n_cases = cas humains (AFP), n_env_samples = echantillons
#     environnementaux, comptes tels qu'ecrits dans le bulletin ; count =
#     n_cases + n_env_samples (nombre de detections, pas de cas).
# Si les fichiers du module sont absents, toutes les lignes passent en
# "review" (count NA) avec un avertissement : le pipeline continue sans
# inventer de nombre.
# ------------------------------------------------------------
.load_count_parser <- function(root_dir) {
  if (exists("segment_atomic_events", mode = "function", envir = .GlobalEnv)) {
    return(TRUE)
  }
  f_norm <- file.path(root_dir, "R", "lib_country_normalize.R")
  f_atom <- file.path(root_dir, "R", "lib_atomic_parser.R")
  if (!file.exists(f_norm) || !file.exists(f_atom)) {
    warning(
      "[POLIO PREP] Module de comptage introuvable (R/lib_country_normalize.R, ",
      "R/lib_atomic_parser.R) : tous les comptages seront NA (a verifier), ",
      "aucun nombre n'est invente.",
      call. = FALSE
    )
    return(FALSE)
  }
  ok <- tryCatch({
    sys.source(f_norm, envir = .GlobalEnv)
    sys.source(f_atom, envir = .GlobalEnv)
    TRUE
  }, error = function(e) {
    warning("[POLIO PREP] Module de comptage non chargeable : ", conditionMessage(e),
            " -> comptages NA (a verifier).", call. = FALSE)
    FALSE
  })
  isTRUE(ok) && exists("segment_atomic_events", mode = "function", envir = .GlobalEnv)
}

.review_virus_labels <- function(x) {
  x0 <- tolower(.clean_txt(x))
  v <- character(0)
  if (stringr::str_detect(x0, "cvdpv1")) v <- c(v, "cVDPV1")
  if (stringr::str_detect(x0, "cvdpv2")) v <- c(v, "cVDPV2")
  if (stringr::str_detect(x0, "cvdpv3")) v <- c(v, "cVDPV3")
  if (stringr::str_detect(x0, "wpv1")) {
    v <- c(v, "WPV1")
  } else if (stringr::str_detect(x0, "wpv")) {
    v <- c(v, "WPV")
  }
  if (length(v) == 0) "Unspecified" else v
}

.count_bullet <- function(raw_bullet, issue_date, source_url, row_id) {
  # scope_text : texte dont on tire virus et type d'evenement pour une ligne a
  # verifier. Par defaut tout le texte ; pour un texte multi-pays, seulement le
  # premier segment (la ligne est rattachee au premier pays), afin de ne pas
  # attribuer a ce pays un virus mentionne pour un autre pays.
  as_review <- function(reason, scope_text = raw_bullet) {
    vt <- .review_virus_labels(scope_text)
    tibble::tibble(
      virus_type = vt,
      n_cases = NA_integer_,
      n_env_samples = NA_integer_,
      count_status = "review",
      count_review_reason = reason,
      signal_kw = rep(detect_signal_type(scope_text), length(vt))
    )
  }
  ev <- tryCatch(
    segment_atomic_events(raw_bullet, issue_date, source_url, row_id),
    error = function(e) e
  )
  if (inherits(ev, "error")) {
    return(as_review(paste0("parser_error: ", conditionMessage(ev))))
  }
  if (is.null(ev) || nrow(ev) == 0) {
    return(as_review("parser_returned_no_rows"))
  }
  seg_id <- sub("_e[0-9]+$", "", ev$atomic_segment_id)
  if (length(unique(seg_id)) != 1L) {
    return(as_review("multiple_segments_in_one_bullet",
                     scope_text = ev$atomic_segment_text[1]))
  }
  if (any(!is.na(ev$exclusion_reason))) {
    return(as_review(paste(unique(stats::na.omit(ev$exclusion_reason)), collapse = " | ")))
  }
  ev |>
    dplyr::group_by(virus_type) |>
    dplyr::summarise(
      n_cases = sum(count[detection_source == "AFP"]),
      n_env_samples = sum(count[detection_source == "Environmental"]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_cases = as.integer(n_cases),
      n_env_samples = as.integer(n_env_samples),
      count_status = "parsed",
      count_review_reason = NA_character_,
      signal_kw = NA_character_
    )
}

# Une ligne par (bulletin x type de virus). Un bulletin a un seul virus (cas
# habituel) garde une seule ligne ; un bulletin a plusieurs virus donne une
# ligne par virus au lieu de n'en garder qu'un seul.
.apply_atomic_counts <- function(df, parser_ok, issue_date, source_url, issue_id) {
  parser_version <- get0("LIB_ATOMIC_PARSER_VERSION", envir = .GlobalEnv,
                         ifnotfound = "unavailable")
  parts <- lapply(seq_len(nrow(df)), function(i) {
    cnt <- if (isTRUE(parser_ok)) {
      .count_bullet(df$raw_bullet[i], issue_date, source_url,
                    paste0(issue_id, "_b", i))
    } else {
      tibble::tibble(
        virus_type = .review_virus_labels(df$raw_bullet[i]),
        n_cases = NA_integer_,
        n_env_samples = NA_integer_,
        count_status = "review",
        count_review_reason = "parser_unavailable",
        signal_kw = detect_signal_type(df$raw_bullet[i])
      )
    }
    base <- df[rep(i, nrow(cnt)), setdiff(names(df), c("virus_type", "count")), drop = FALSE]
    dplyr::bind_cols(base, cnt)
  })
  out <- dplyr::bind_rows(parts)
  parsed <- out$count_status == "parsed"
  kw_signal <- out$signal_kw
  out$signal_type <- ifelse(
    parsed,
    dplyr::case_when(
      out$n_cases > 0 & out$n_env_samples > 0 ~ "Case + Environmental",
      out$n_cases > 0 ~ "Case",
      out$n_env_samples > 0 ~ "Environmental",
      TRUE ~ "Unspecified"
    ),
    kw_signal
  )
  out$event_type <- out$signal_type
  out$count <- ifelse(parsed, out$n_cases + out$n_env_samples, NA_integer_)
  out$count <- as.integer(out$count)
  out$signal_kw <- NULL
  out$parser_version <- parser_version
  out
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

  # CORRECTIF 09/08/2026 -- BUG RACINE TROUVE EN PRODUCTION (recurrent,
  # deja rencontre au moins une fois avant avec une autre date fausse :
  # voir le garde-fou "20 June 2026" plus bas dans ce fichier, ajoute a
  # l'epoque comme rustine sans jamais corriger la cause).
  #
  # parse_all_dates_non_future() scanne TOUT le texte de la page (y
  # compris chaque date d'apparition de paralysie / d'echantillon le
  # plus recent mentionnee pour chaque pays dans les sections
  # detaillees), retient toutes les dates non-futures, et prend la PLUS
  # RECENTE. Si aucune de ces dates de cas individuelles n'est aussi
  # recente que la vraie date "as of" du bulletin (frequent : un pays
  # peut ne pas avoir eu de nouveau cas depuis des mois), cette date de
  # cas ancienne gagne a tort -- exactement ce qui produisait "20 May
  # 2026" alors que le bulletin etait celui du 05 August 2026.
  #
  # 04_run_polio_pipeline_core.R determine DEJA la vraie date via
  # check_polio_update() (qui lit directement l'entete "as of DATE" de
  # la page, fiable) et la transmet ici via le parametre latest_issue --
  # mais ce parametre etait declare et jamais utilise. On lui donne
  # maintenant la priorite ; l'ancienne heuristique ne sert plus que de
  # filet de secours si la fonction est appelee seule, sans latest_issue
  # (usage direct/manuel, hors du pipeline normal).
  reporting_date <- NA
  if (!is.null(latest_issue) && !is.null(latest_issue$issue_date) &&
      nzchar(as.character(latest_issue$issue_date))) {
    reporting_date <- as.Date(as.character(latest_issue$issue_date), format = "%d %B %Y")
    if (is.na(reporting_date)) {
      reporting_date <- suppressWarnings(as.Date(as.character(latest_issue$issue_date)))
    }
    if (!is.na(reporting_date)) {
      message("[POLIO PREP] Reporting date pris depuis latest_issue (fiable) : ", latest_issue$issue_date)
    }
  }

  if (is.na(reporting_date)) {
    message("[POLIO PREP] latest_issue absent/invalide -> repli sur l'heuristique de scan de texte.")
    reporting_date <- parse_all_dates_non_future(
      x = gpei$text,
      extraction_date = extraction_date
    )
  }

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

  # Garde-fou general (en plus du cas specifique deja code plus bas) :
  # si la date retenue accuse plus de 21 jours de retard sur la date
  # d'extraction, c'est le signe probable du meme bug sous une autre
  # forme -- on ne bloque pas le pipeline pour autant, mais on le
  # signale bien fort dans les logs.
  if (!is.na(reporting_date) && as.numeric(extraction_date - reporting_date) > 21) {
    warning(
      "[POLIO PREP] ATTENTION : reporting_date (", reporting_date, ") a plus de 21 jours ",
      "de retard sur l'extraction (", extraction_date, "). Verifier qu'il ne s'agit pas ",
      "a nouveau du bug de date extraite depuis le texte des cas individuels.",
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
      count = NA_integer_,   # remplace par le comptage atomique ci-dessous
      location_text = NA_character_,
      onset_date = NA_character_,
      pathogen = "Polio virus",
      geo_level = "country"
    )
  
  # Comptage atomique (27 oct. 2026) : voir .load_count_parser()
  parser_ok <- .load_count_parser(root_dir)
  df <- .apply_atomic_counts(
    df,
    parser_ok = parser_ok,
    issue_date = as.character(reporting_date),
    source_url = url,
    issue_id = issue_id
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
      n_cases,
      n_env_samples,
      count_status,
      count_review_reason,
      parser_version,
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
  n_review <- sum(out$count_status == "review", na.rm = TRUE)
  message("[POLIO PREP] Counting: ", sum(out$count_status == "parsed", na.rm = TRUE),
          " row(s) parsed, ", n_review, " row(s) to verify (count = NA, never 1).")
  if (n_review > 0) {
    warning("[POLIO PREP] ", n_review, " row(s) with a count to verify: ",
            paste(unique(paste0(out$country_raw, " [", out$count_review_reason, "]")[out$count_status == "review"]),
                  collapse = "; "),
            call. = FALSE)
  }
  
  invisible(out)
}