# =========================================================
# R/09_send_polio_rcc_emails.R
# PREIS_Polio_FV - RCC conditional email sender
# DEFINITIVE FIX: reporting date always validated from CSV
# =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# ---------------------------------------------------------
# Helpers
# ---------------------------------------------------------
.clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

.safe_col <- function(df, col, default = "") {
  if (col %in% names(df)) return(df[[col]])
  rep(default, nrow(df))
}

.parse_valid_email_reporting_date <- function(alerts) {
  
  if (is.null(alerts) || nrow(alerts) == 0) {
    stop("[EMAIL DATE] Empty alerts dataset. Cannot determine reporting date.")
  }
  
  candidate_values <- c()
  
  if ("reporting_date" %in% names(alerts)) {
    candidate_values <- c(candidate_values, as.character(alerts$reporting_date))
  }
  
  if ("issue_date" %in% names(alerts)) {
    candidate_values <- c(candidate_values, as.character(alerts$issue_date))
  }
  
  # report_date is used last because it is a label, not the safest date field
  if ("report_date" %in% names(alerts)) {
    candidate_values <- c(candidate_values, as.character(alerts$report_date))
  }
  
  if ("reporting_date_label" %in% names(alerts)) {
    candidate_values <- c(candidate_values, as.character(alerts$reporting_date_label))
  }
  
  candidate_values <- .clean_text(candidate_values)
  candidate_values <- candidate_values[nzchar(candidate_values)]
  
  if (length(candidate_values) == 0) {
    stop("[EMAIL DATE] No reporting date candidate found in alerts file.")
  }
  
  parsed <- suppressWarnings(lubridate::parse_date_time(
    candidate_values,
    orders = c("Y-m-d", "d B Y", "B d Y", "d b Y", "b d Y"),
    locale = "C"
  ))
  
  parsed <- as.Date(parsed)
  parsed <- parsed[!is.na(parsed)]
  parsed <- parsed[parsed <= Sys.Date()]
  
  if (length(parsed) == 0) {
    stop("[EMAIL DATE] No valid non-future reporting date found. Email stopped.")
  }
  
  final_date <- max(parsed)
  
  final_label <- format(final_date, "%d %B %Y")
  
  if (final_date > Sys.Date()) {
    stop("[EMAIL DATE] Future reporting date detected. Email stopped.")
  }
  
  if (grepl("20 June 2026", final_label, ignore.case = TRUE)) {
    stop("[EMAIL DATE] Wrong reporting date '20 June 2026' still detected. Email stopped.")
  }
  
  list(
    date = final_date,
    label = final_label
  )
}

collapse_alert_lines <- function(df, max_n = 10) {
  if (is.null(df) || nrow(df) == 0) {
    return("No alerts available.")
  }
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n_take <- min(nrow(df), max_n)
  lines <- character(n_take)
  
  for (i in seq_len(n_take)) {
    country_i <- if ("country" %in% names(df)) .clean_text(df$country[i]) else ""
    rcc_i <- if ("rcc" %in% names(df)) .clean_text(df$rcc[i]) else ""
    summary_i <- if ("summary_text" %in% names(df)) .clean_text(df$summary_text[i]) else ""
    raw_i <- if ("raw_bullet" %in% names(df)) .clean_text(df$raw_bullet[i]) else ""
    pathogen_i <- if ("pathogen" %in% names(df)) .clean_text(df$pathogen[i]) else ""
    virus_i <- if ("virus_type" %in% names(df)) .clean_text(df$virus_type[i]) else ""
    signal_i <- if ("signal_type" %in% names(df)) .clean_text(df$signal_type[i]) else ""
    url_i <- if ("source_url" %in% names(df)) .clean_text(df$source_url[i]) else ""
    
    main_txt <- summary_i
    if (!nzchar(main_txt)) main_txt <- raw_i
    if (!nzchar(main_txt)) main_txt <- "Polio-related event identified"
    
    bits <- c()
    if (nzchar(country_i)) bits <- c(bits, paste0("Country: ", country_i))
    if (nzchar(rcc_i)) bits <- c(bits, paste0("RCC: ", rcc_i))
    if (nzchar(pathogen_i)) bits <- c(bits, paste0("Pathogen: ", pathogen_i))
    if (nzchar(virus_i)) bits <- c(bits, paste0("Virus type: ", virus_i))
    if (nzchar(signal_i)) bits <- c(bits, paste0("Signal type: ", signal_i))
    
    meta <- if (length(bits) > 0) paste0(" (", paste(bits, collapse = " | "), ")") else ""
    url_txt <- if (nzchar(url_i)) paste0("\n   Source: ", url_i) else ""
    
    lines[i] <- paste0(i, ". ", main_txt, meta, url_txt)
  }
  
  extra_txt <- ""
  if (nrow(df) > max_n) {
    extra_txt <- paste0("\n\nAdditional alerts not shown: ", nrow(df) - max_n)
  }
  
  paste0(paste(lines, collapse = "\n\n"), extra_txt)
}

build_polio_email_body <- function(
    rcc_name,
    df,
    recipient_name = "Colleague",
    issue_date_label = NULL,
    recipient_mode = "SUMMARY"
) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n_alerts <- nrow(df)
  
  if (is.null(issue_date_label) || is.na(issue_date_label) || !nzchar(.clean_text(issue_date_label))) {
    date_obj <- .parse_valid_email_reporting_date(df)
    issue_date_label <- date_obj$label
  }
  
  if (grepl("20 June 2026", issue_date_label, ignore.case = TRUE)) {
    stop("[EMAIL BODY] Wrong reporting date detected in body. Email stopped.")
  }
  
  source_link <- "https://polioeradication.org/about-polio/polio-this-week/"
  if ("source_url" %in% names(df)) {
    src <- unique(.clean_text(df$source_url))
    src <- src[nzchar(src)]
    if (length(src) > 0) source_link <- src[1]
  }
  
  affected_countries <- 0L
  if ("country" %in% names(df)) {
    affected_countries <- length(unique(.clean_text(df$country)[.clean_text(df$country) != ""]))
  }
  
  intro <- if (tolower(.clean_text(rcc_name)) == "all") {
    paste0(
      "Dear ", recipient_name, ",\n\n",
      "Please find below the weekly automated PREIS POLIO update for Africa, ",
      "based on the latest information published in the Global Polio Eradication Initiative weekly report dated ",
      issue_date_label, ".\n\n",
      "Dashboard access: https://zrhyacinthepreis26.shinyapps.io/dashboard/\n\n"
    )
  } else {
    paste0(
      "Dear ", recipient_name, ",\n\n",
      "Please find below the weekly automated PREIS POLIO update for the ",
      rcc_name,
      " Regional Coordination Centre (RCC), based on the latest information published in the Global Polio Eradication Initiative weekly report dated ",
      issue_date_label, ".\n\n",
      "Dashboard access: https://zrhyacinthepreis26.shinyapps.io/dashboard/\n\n"
    )
  }
  
  extraction_txt <- ""
  if ("extracted_at" %in% names(df)) {
    ext <- unique(.clean_text(df$extracted_at))
    ext <- ext[nzchar(ext)]
    if (length(ext) > 0) {
      extraction_txt <- paste0(
        "Extraction date: ", ext[1],
        "\nSource: Global Polio Eradication Initiative weekly update\n",
        source_link, "\n\n"
      )
    }
  }
  
  summary_block <- paste0(
    "Summary\n",
    "- Number of polio-related events identified: ", n_alerts, "\n",
    "- Number of affected countries: ", affected_countries, "\n",
    "- Reporting date: ", issue_date_label, "\n\n"
  )
  
  detail <- if (n_alerts > 0) {
    section_title <- if (toupper(.clean_text(recipient_mode)) == "FULL") {
      "Detailed events identified:\n\n"
    } else {
      "Events identified:\n\n"
    }
    paste0(section_title, collapse_alert_lines(df, max_n = 10), "\n\n")
  } else {
    "No alert was identified for this RCC in the current dataset.\n\n"
  }
  
  outro <- paste0(
    "This message was generated automatically by the PREIS POLIO surveillance workflow.\n",
    "Please consult the dashboard and the source document above for the full weekly report and contextual details.\n\n",
    "Best regards,\n",
    "PREIS POLIO Automated Intelligence System\n",
    "Surveillance and Disease Intelligence Division\n",
    "Africa CDC\n\n",
    "--\n",
    "Ce message a \u00e9t\u00e9 produit automatiquement par PREIS du Dr Hyacinthe ZABR\u00c9, Prof."
  )
  
  paste0(intro, extraction_txt, summary_block, detail, outro)
}

# ---------------------------------------------------------
# Main conditional sender
# ---------------------------------------------------------
send_polio_rcc_emails_conditional <- function(send_now = FALSE, force_send = FALSE) {
  message("[Polio_FV] Conditional RCC email sender start")
  
  input_file <- "dashboard/data/dashboard/polio_africa_email_input.csv"
  recip_file <- "dashboard/data/dashboard/alert_recipients.csv"
  
  if (!file.exists(input_file)) {
    stop("Input file not found: ", input_file)
  }
  
  if (!file.exists(recip_file)) {
    stop("Recipients file not found: ", recip_file)
  }
  
  if (!exists("send_email_safely", mode = "function")) {
    stop("Function send_email_safely() not found. Load your email core before sending.")
  }
  
  alerts <- readr::read_csv(input_file, show_col_types = FALSE)
  recips <- readr::read_csv(recip_file, show_col_types = FALSE)
  
  if (!"email" %in% names(recips)) {
    stop("alert_recipients.csv must contain column: email")
  }
  if (!"rcc" %in% names(recips)) {
    stop("alert_recipients.csv must contain column: rcc")
  }
  if (!"rcc" %in% names(alerts)) {
    stop("polio_africa_email_input.csv must contain column: rcc")
  }
  
  if (!"name" %in% names(recips)) recips$name <- "Colleague"
  if (!"mode" %in% names(recips)) recips$mode <- "SUMMARY"
  
  recips <- recips %>%
    mutate(
      email = .clean_text(email),
      rcc   = .clean_text(rcc),
      name  = .clean_text(name),
      mode  = .clean_text(mode),
      rcc   = ifelse(!nzchar(rcc), "All", rcc),
      name  = ifelse(!nzchar(name), "Colleague", name),
      mode  = ifelse(!nzchar(mode), "SUMMARY", mode)
    ) %>%
    filter(nzchar(email))
  
  alerts <- alerts %>%
    mutate(
      rcc = .clean_text(rcc),
      rcc = ifelse(!nzchar(rcc), "Unspecified", rcc)
    )
  
  date_obj <- .parse_valid_email_reporting_date(alerts)
  issue_date_label <- date_obj$label
  
  message("[Polio_FV] Reporting date used in email: ", issue_date_label)
  
  if (grepl("20 June 2026", issue_date_label, ignore.case = TRUE)) {
    stop("[Polio_FV] Wrong reporting date detected before sending. Email stopped.")
  }
  
  out <- vector("list", nrow(recips))
  
  for (i in seq_len(nrow(recips))) {
    recipient_email <- recips$email[i]
    recipient_rcc   <- recips$rcc[i]
    recipient_name  <- recips$name[i]
    recipient_mode  <- recips$mode[i]
    
    is_all_recipient <- tolower(recipient_rcc) == "all"
    
    subset_alerts <- if (is_all_recipient) {
      alerts
    } else {
      alerts %>% filter(tolower(rcc) == tolower(recipient_rcc))
    }
    
    n_alerts <- nrow(subset_alerts)
    
    if (!is_all_recipient && n_alerts == 0) {
      status <- if (isTRUE(send_now)) "skipped_no_alert" else "dry_run_no_alert"
      
      message(
        "[Polio_FV] ", recipient_email,
        " | RCC=", recipient_rcc,
        " | alerts=", n_alerts,
        " | status=", status
      )
      
      out[[i]] <- data.frame(
        email = recipient_email,
        rcc = recipient_rcc,
        n_alerts = as.numeric(n_alerts),
        status = status,
        stringsAsFactors = FALSE
      )
      next
    }
    
    if (is_all_recipient && n_alerts == 0 && !isTRUE(force_send)) {
      status <- if (isTRUE(send_now)) "skipped_no_alert" else "dry_run_no_alert"
      
      message(
        "[Polio_FV] ", recipient_email,
        " | RCC=", recipient_rcc,
        " | alerts=", n_alerts,
        " | status=", status
      )
      
      out[[i]] <- data.frame(
        email = recipient_email,
        rcc = recipient_rcc,
        n_alerts = as.numeric(n_alerts),
        status = status,
        stringsAsFactors = FALSE
      )
      next
    }
    
    subject <- if (is_all_recipient) {
      paste0("PREIS POLIO Weekly Update – Africa – ", issue_date_label)
    } else {
      paste0("PREIS POLIO Weekly Update – ", recipient_rcc, " RCC – ", issue_date_label)
    }
    
    body <- build_polio_email_body(
      rcc_name = recipient_rcc,
      df = subset_alerts,
      recipient_name = recipient_name,
      issue_date_label = issue_date_label,
      recipient_mode = recipient_mode
    )
    
    if (grepl("20 June 2026", body, ignore.case = TRUE)) {
      stop("[Polio_FV] Wrong reporting date found inside email body. Email stopped.")
    }
    
    email_res <- send_email_safely(
      to = recipient_email,
      subject = subject,
      body = body,
      send_now = send_now
    )
    
    status <- if (!is.null(email_res$status)) as.character(email_res$status) else "unknown"
    
    message(
      "[Polio_FV] ", recipient_email,
      " | RCC=", recipient_rcc,
      " | alerts=", n_alerts,
      " | status=", status
    )
    
    out[[i]] <- data.frame(
      email = recipient_email,
      rcc = recipient_rcc,
      n_alerts = as.numeric(n_alerts),
      status = status,
      stringsAsFactors = FALSE
    )
  }
  
  result <- dplyr::bind_rows(out)
  
  message("[Polio_FV] Conditional RCC email sender done")
  result
}

stopifnot(exists("send_polio_rcc_emails_conditional", mode = "function"))
stopifnot(exists("build_polio_email_body", mode = "function"))