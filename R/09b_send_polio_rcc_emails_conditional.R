# ============================================================
# PREIS POLIO — Conditional RCC Email Sender
# File: R/09b_send_polio_rcc_emails_conditional.R
# Purpose:
#   Send RCC emails only when at least one polio event is detected
#   in the RCC Member States, and send All/HQ continental summary.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

send_polio_rcc_emails_conditional <- function(
    send_now = FALSE,
    force_send = FALSE,
    project_dir = "D:/PREIS_Polio_FV",
    alerts_path = "dashboard/data/dashboard/polio_africa_email_input.csv",
    recipients_path = "dashboard/data/dashboard/alert_recipients.csv",
    dashboard_url = "https://YOUR_DEPLOYED_PREIS_POLIO_DASHBOARD_URL",
    source_link_default = "https://polioeradication.org/about-polio/polio-this-week/"
) {

  message("[POLIO] Conditional RCC email sender start")

  # ------------------------------------------------------------
  # 0. Resolve paths safely
  # ------------------------------------------------------------
  if (!dir.exists(project_dir)) {
    project_dir <- getwd()
  }

  make_abs <- function(x) {
    if (grepl("^[A-Za-z]:[/\\\\]|^/", x)) {
      return(x)
    }
    file.path(project_dir, x)
  }

  alerts_path <- make_abs(alerts_path)
  recipients_path <- make_abs(recipients_path)
  email_core_path <- file.path(project_dir, "R", "60_email.R")

  if (!file.exists(alerts_path)) {
    stop("alerts file not found: ", alerts_path, call. = FALSE)
  }

  if (!file.exists(recipients_path)) {
    stop("recipients file not found: ", recipients_path, call. = FALSE)
  }

  # Load email core automatically only if present.
  # This does not break dry_run mode if 60_email.R is absent.
  if (file.exists(email_core_path)) {
    source(email_core_path)
  } else if (isTRUE(send_now)) {
    stop("Email core script not found: ", email_core_path, call. = FALSE)
  }

  # ------------------------------------------------------------
  # 1. Read inputs
  # ------------------------------------------------------------
  alerts <- readr::read_csv(alerts_path, show_col_types = FALSE)
  recips <- readr::read_csv(recipients_path, show_col_types = FALSE)

  names(alerts) <- tolower(names(alerts))
  names(recips) <- tolower(names(recips))

  required_alert_cols <- c("rcc")
  miss_alert <- setdiff(required_alert_cols, names(alerts))
  if (length(miss_alert) > 0) {
    stop("alerts file missing columns: ", paste(miss_alert, collapse = ", "), call. = FALSE)
  }

  required_recip_cols <- c("email", "rcc")
  miss_recip <- setdiff(required_recip_cols, names(recips))
  if (length(miss_recip) > 0) {
    stop("recipients file missing columns: ", paste(miss_recip, collapse = ", "), call. = FALSE)
  }

  # Optional recipient columns
  if (!"name" %in% names(recips)) recips$name <- "Colleague"
  if (!"mode" %in% names(recips)) recips$mode <- "SUMMARY"

  # ------------------------------------------------------------
  # 2. Clean inputs
  # ------------------------------------------------------------
  alerts <- alerts %>%
    mutate(
      rcc = str_trim(as.character(rcc))
    ) %>%
    filter(!is.na(rcc), rcc != "")

  recips <- recips %>%
    mutate(
      email = str_trim(as.character(email)),
      rcc   = str_trim(as.character(rcc)),
      name  = ifelse(is.na(name), "Colleague", str_trim(as.character(name))),
      mode  = ifelse(is.na(mode), "SUMMARY", str_trim(as.character(mode)))
    ) %>%
    filter(!is.na(email), email != "", !is.na(rcc), rcc != "")

  if (nrow(recips) == 0) {
    message("[POLIO] No valid recipients found.")
    return(invisible(tibble()))
  }

  if (nrow(alerts) == 0) {
    message("[POLIO] No alerts found. Nothing to send.")

    out <- recips %>%
      transmute(
        email = email,
        rcc = rcc,
        n_alerts = 0L,
        status = "skipped_no_alert"
      )

    out_path <- file.path(project_dir, "dashboard/data/dashboard/polio_email_send_log.csv")
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out, out_path)

    return(invisible(out))
  }

  # ------------------------------------------------------------
  # 3. Derive report date and extraction date
  # ------------------------------------------------------------
  # Reporting date must be the true GPEI weekly report date.
  issue_label <- NA_character_

  if ("report_date" %in% names(alerts) &&
      any(!is.na(alerts$report_date) & str_trim(as.character(alerts$report_date)) != "")) {
    issue_label <- unique(as.character(alerts$report_date[
      !is.na(alerts$report_date) & str_trim(as.character(alerts$report_date)) != ""
    ]))[1]
  } else if ("issue_date" %in% names(alerts) &&
             any(!is.na(alerts$issue_date) & str_trim(as.character(alerts$issue_date)) != "")) {
    issue_label <- unique(as.character(alerts$issue_date[
      !is.na(alerts$issue_date) & str_trim(as.character(alerts$issue_date)) != ""
    ]))[1]
  } else {
    issue_label <- format(Sys.Date(), "%d %B %Y")
  }

  # Robust date formatting.
  # Accepts ISO dates (2026-04-22), English text dates (22 April 2026),
  # and already formatted labels. It never stops the pipeline if parsing fails.
  format_issue_label <- function(x) {
    x <- as.character(x)[1]
    x <- stringr::str_squish(x)

    if (is.na(x) || x == "") {
      return(format(Sys.Date(), "%d %B %Y"))
    }

    # Remove possible prefixes sometimes found in extracted text.
    x_clean <- x
    x_clean <- stringr::str_replace_all(x_clean, "(?i)^report(ing)?\\s*date\\s*[:=]\\s*", "")
    x_clean <- stringr::str_replace_all(x_clean, "(?i)^gpei\\s*report\\s*dated\\s*", "")
    x_clean <- stringr::str_squish(x_clean)

    # Try lubridate if available.
    if (requireNamespace("lubridate", quietly = TRUE)) {
      parsed <- suppressWarnings(lubridate::parse_date_time(
        x_clean,
        orders = c(
          "ymd", "dmy", "mdy",
          "d B Y", "d b Y",
          "B d Y", "b d Y",
          "d-B-Y", "d-b-Y",
          "Y-m-d", "d/m/Y", "m/d/Y"
        ),
        locale = Sys.getlocale("LC_TIME"),
        quiet = TRUE
      ))
      if (length(parsed) > 0 && !is.na(parsed[1])) {
        parsed_date <- as.Date(parsed[1])
        # CORRECTION : date future rejetee -> remplacee par date du jour
        if (!is.na(parsed_date) && parsed_date > Sys.Date()) {
          message(
            "[POLIO EMAIL] WARNING: future date detected in report_date ('", x, "'). ",
            "Replaced by extraction date: ", format(Sys.Date(), "%d %B %Y")
          )
          return(format(Sys.Date(), "%d %B %Y"))
        }
        return(format(parsed_date, "%d %B %Y"))
      }
    }

    # Base R fallback with explicit formats.
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
      "%d %B %Y", "%d %b %Y",
      "%B %d %Y", "%b %d %Y",
      "%d-%B-%Y", "%d-%b-%Y"
    )

    for (fmt in formats) {
      parsed <- suppressWarnings(tryCatch(as.Date(x_clean, format = fmt), error = function(e) NA))
      if (!is.na(parsed)) {
        # CORRECTION : date future rejetee -> remplacee par date du jour
        if (parsed > Sys.Date()) {
          message(
            "[POLIO EMAIL] WARNING: future date detected in report_date ('", x, "'). ",
            "Replaced by extraction date: ", format(Sys.Date(), "%d %B %Y")
          )
          return(format(Sys.Date(), "%d %B %Y"))
        }
        return(format(parsed, "%d %B %Y"))
      }
    }

    # Final fallback: keep original text, do not crash.
    x_clean
  }

  issue_label <- format_issue_label(issue_label)

  extraction_label <- format(Sys.time(), "%d %B %Y %H:%M:%S %Z")

  # Global source link if available
  source_link <- source_link_default
  if ("source_url" %in% names(alerts)) {
    src <- unique(as.character(alerts$source_url[
      !is.na(alerts$source_url) & str_trim(as.character(alerts$source_url)) != ""
    ]))
    if (length(src) > 0) source_link <- src[1]
  }

  # Dashboard link
  dashboard_url <- str_trim(as.character(dashboard_url))
  if (is.na(dashboard_url) || dashboard_url == "") {
    dashboard_url <- "https://zrhyacinthepreis26.shinyapps.io/dashboard/"
  }

  # ------------------------------------------------------------
  # 4. Prepare and send emails
  # ------------------------------------------------------------
  send_log <- vector("list", nrow(recips))

  for (i in seq_len(nrow(recips))) {

    recip_email <- recips$email[i]
    recip_rcc   <- recips$rcc[i]
    recip_name  <- recips$name[i]
    recip_mode  <- toupper(recips$mode[i])

    is_all_recipient <- tolower(recip_rcc) %in% c("all", "hq", "continental")

    sub_alerts <- if (is_all_recipient) {
      alerts
    } else {
      alerts %>% filter(tolower(rcc) == tolower(recip_rcc))
    }

    n_sub <- nrow(sub_alerts)

    # Business rule:
    # - RCC-specific recipients: send only when there are alerts for their RCC
    # - All/HQ recipients: send continental summary when alerts exist
    # - force_send=TRUE allows sending even when there are zero alerts
    if (!is_all_recipient && n_sub == 0 && !isTRUE(force_send)) {
      message("[POLIO] Skip ", recip_email, " | RCC=", recip_rcc, " | no alerts")
      send_log[[i]] <- tibble(
        email = recip_email,
        rcc = recip_rcc,
        n_alerts = 0L,
        status = "skipped_no_alert"
      )
      next
    }

    if (is_all_recipient && n_sub == 0 && !isTRUE(force_send)) {
      message("[POLIO] Skip ", recip_email, " | RCC=All | no alerts")
      send_log[[i]] <- tibble(
        email = recip_email,
        rcc = recip_rcc,
        n_alerts = 0L,
        status = "skipped_no_alert"
      )
      next
    }

    subject <- if (is_all_recipient) {
      paste0("PREIS POLIO Weekly Update – Africa – GPEI Report ", issue_label)
    } else {
      paste0("PREIS POLIO Weekly Update – ", recip_rcc, " RCC – GPEI Report ", issue_label)
    }

    # Build summary counts by RCC
    cnt <- sub_alerts %>%
      count(rcc, name = "n") %>%
      arrange(rcc)

    # Build detailed lines
    detail_lines <- character(0)

    if (n_sub > 0) {
      detail_lines <- apply(as.data.frame(sub_alerts), 1, function(r) {

        country <- if ("country" %in% names(sub_alerts) &&
                       !is.na(r[["country"]]) &&
                       nzchar(str_trim(as.character(r[["country"]]))) ) {
          as.character(r[["country"]])
        } else {
          "Unspecified country"
        }

        rcc_lab <- if (!is.na(r[["rcc"]]) &&
                       nzchar(str_trim(as.character(r[["rcc"]]))) ) {
          as.character(r[["rcc"]])
        } else {
          "Unspecified"
        }

        summary_txt <- if ("summary_text" %in% names(sub_alerts) &&
                           !is.na(r[["summary_text"]]) &&
                           nzchar(str_trim(as.character(r[["summary_text"]]))) ) {
          as.character(r[["summary_text"]])
        } else if ("raw_bullet" %in% names(sub_alerts) &&
                   !is.na(r[["raw_bullet"]]) &&
                   nzchar(str_trim(as.character(r[["raw_bullet"]]))) ) {
          as.character(r[["raw_bullet"]])
        } else {
          "Polio-related event identified"
        }

        virus_type <- if ("virus_type" %in% names(sub_alerts) &&
                          !is.na(r[["virus_type"]]) &&
                          nzchar(str_trim(as.character(r[["virus_type"]]))) ) {
          as.character(r[["virus_type"]])
        } else {
          ""
        }

        signal_type <- if ("signal_type" %in% names(sub_alerts) &&
                           !is.na(r[["signal_type"]]) &&
                           nzchar(str_trim(as.character(r[["signal_type"]]))) ) {
          as.character(r[["signal_type"]])
        } else {
          ""
        }

        suffix <- c()
        if (nzchar(virus_type)) suffix <- c(suffix, virus_type)
        if (nzchar(signal_type)) suffix <- c(suffix, signal_type)

        paste0(
          "- ", country, " (", rcc_lab, " RCC): ", summary_txt,
          if (length(suffix) > 0) paste0(" [", paste(suffix, collapse = " | "), "]") else ""
        )
      })
    }

    greeting_name <- ifelse(
      is.na(recip_name) || !nzchar(str_trim(as.character(recip_name))),
      "Colleague",
      recip_name
    )

    n_countries <- if ("country" %in% names(sub_alerts)) {
      length(unique(sub_alerts$country[
        !is.na(sub_alerts$country) & str_trim(as.character(sub_alerts$country)) != ""
      ]))
    } else {
      NA_integer_
    }

    scope_sentence <- if (is_all_recipient) {
      paste0(
        "Please find below the weekly automated PREIS POLIO update for Africa, ",
        "based on the latest information published in the Global Polio Eradication Initiative weekly report dated ",
        issue_label, "."
      )
    } else {
      paste0(
        "Please find below the weekly automated PREIS POLIO update for the ",
        recip_rcc,
        " Regional Coordination Centre (RCC), based on the latest information published in the Global Polio Eradication Initiative weekly report dated ",
        issue_label, "."
      )
    }

    body_lines <- c(
      paste0("Dear ", greeting_name, ","),
      "",
      scope_sentence,
      "",
      "Dashboard access:",
      dashboard_url,
      "",
      paste0("Extraction date: ", extraction_label),
      paste0("Source: Global Polio Eradication Initiative weekly update"),
      source_link,
      "",
      "Summary",
      "",
      paste0("- Number of polio-related events identified: ", n_sub),
      if (!is.na(n_countries)) paste0("- Number of affected countries: ", n_countries) else NULL,
      paste0("- Reporting date: ", issue_label),
      ""
    )

    if (nrow(cnt) > 0) {
      body_lines <- c(
        body_lines,
        "Alert counts by RCC",
        "",
        paste0("- ", cnt$rcc, ": ", cnt$n),
        ""
      )
    }

    if (n_sub > 0) {
      section_title <- if (is_all_recipient || recip_mode == "FULL") {
        "Detailed events identified"
      } else {
        "Events identified for your RCC"
      }

      body_lines <- c(
        body_lines,
        section_title,
        "",
        detail_lines,
        ""
      )
    }

    body_lines <- c(
      body_lines,
      "This message was generated automatically by the PREIS POLIO surveillance workflow.",
      "Please consult the dashboard and the source document above for the full weekly report and contextual details.",
      "",
      "Best regards,",
      "PREIS POLIO Automated Intelligence System",
      "Surveillance and Disease Intelligence Division",
      "Africa CDC",
      "",
      "--",
      "Ce message a \u00e9t\u00e9 produit automatiquement par PREIS du Dr Hyacinthe ZABR\u00c9, Prof."
    )

    body <- paste(body_lines, collapse = "\n")

    status <- "dry_run"

    if (isTRUE(send_now)) {
      if (!exists("send_email_safely", mode = "function")) {
        stop("Function send_email_safely() not found. Check R/60_email.R.", call. = FALSE)
      }

      res <- tryCatch(
        {
          send_email_safely(
            to = recip_email,
            subject = subject,
            body = body,
            send_now = TRUE
          )
        },
        error = function(e) {
          list(
            success = FALSE,
            status = "failed",
            error = conditionMessage(e)
          )
        }
      )

      status <- if (isTRUE(res$success)) {
        if (!is.null(res$status) && nzchar(res$status)) res$status else "sent"
      } else {
        paste0("failed: ", if (!is.null(res$error)) res$error else "unknown error")
      }
    }

    message(
      "[POLIO] ", recip_email,
      " | RCC=", recip_rcc,
      " | alerts=", n_sub,
      " | status=", status
    )

    send_log[[i]] <- tibble(
      email = recip_email,
      rcc = recip_rcc,
      n_alerts = n_sub,
      status = status
    )
  }

  out <- bind_rows(send_log)

  out_path <- file.path(project_dir, "dashboard/data/dashboard/polio_email_send_log.csv")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, out_path)

  message("[POLIO] Conditional RCC email sender done")
  invisible(out)
}
