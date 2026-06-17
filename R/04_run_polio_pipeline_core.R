# ============================================================
# R/04_run_polio_pipeline_core.R — PREIS POLIO Core Pipeline
# Version robuste sans casser les anciens scripts
# ============================================================

run_polio_pipeline_core <- function(
    send_now = TRUE,
    force_send = FALSE,
    project_dir = "D:/PREIS_Polio_FV",
    dashboard_url = "https://zrhyacinthepreis26.shinyapps.io/dashboard/"
) {
  
  ROOT <- project_dir
  assign("ROOT", ROOT, envir = .GlobalEnv)
  
  result <- list(
    status = "STARTED",
    latest_issue = NULL,
    prepared = FALSE,
    sent = FALSE,
    prepare_output = NULL,
    send_output = NULL
  )
  
  message("[POLIO CORE] ROOT: ", ROOT)
  
  # ------------------------------------------------------------
  # 0. Charger les scripts requis
  # ------------------------------------------------------------
  scripts_required <- c(
    "R/02_check_polio_update.R",
    "R/05_prepare_polio_alert_input.R",
    "R/60_email.R",
    "R/09b_send_polio_rcc_emails_conditional.R"
  )
  
  for (f in scripts_required) {
    fp <- file.path(ROOT, f)
    if (!file.exists(fp)) {
      stop("[POLIO CORE] Missing required script: ", fp)
    }
    source(fp, local = .GlobalEnv)
    message("[POLIO CORE] Loaded: ", f)
  }
  
  # ------------------------------------------------------------
  # 1. Vérifier fonctions obligatoires
  # ------------------------------------------------------------
  required_functions <- c(
    "check_polio_update",
    "prepare_polio_alert_input",
    "send_email_safely",
    "send_polio_rcc_emails_conditional"
  )
  
  for (fn in required_functions) {
    if (!exists(fn, mode = "function", envir = .GlobalEnv)) {
      stop("[POLIO CORE] Required function not found: ", fn)
    }
  }
  
  # ------------------------------------------------------------
  # 2. Détecter la DERNIÈRE issue depuis GPEI
  #    Important: on parse la sortie "Current", pas l'ancien CSV.
  # ------------------------------------------------------------
  message("[POLIO CORE] STEP 1 — Check latest GPEI Polio This Week issue")
  
  check_output <- NULL
  check_result <- NULL
  
  check_output <- capture.output({
    check_result <- check_polio_update()
  })
  
  cat(paste(check_output, collapse = "\n"), "\n")
  
  current_line <- check_output[grepl("^Current\\s*:", check_output)]
  previous_line <- check_output[grepl("^Previous\\s*:", check_output)]
  
  current_issue_id <- NA_character_
  
  if (length(current_line) > 0) {
    current_issue_id <- trimws(sub("^Current\\s*:\\s*", "", current_line[length(current_line)]))
  }
  
  if (is.na(current_issue_id) || current_issue_id == "") {
    
    if (is.list(check_result) && !is.null(check_result$issue_id)) {
      current_issue_id <- as.character(check_result$issue_id)
    } else if (is.list(check_result) && !is.null(check_result$current_issue)) {
      current_issue_id <- as.character(check_result$current_issue)
    } else {
      stop("[POLIO CORE] Unable to identify current GPEI issue_id from check_polio_update().")
    }
  }
  
  issue_date <- current_issue_id
  issue_date <- sub("^polio_this_week__?", "", issue_date)
  issue_date <- gsub("_", " ", issue_date)
  issue_date <- tools::toTitleCase(issue_date)
  
  latest_issue <- list(
    issue_id = current_issue_id,
    issue_date = issue_date,
    source_url = "https://polioeradication.org/about-polio/polio-this-week/",
    extracted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  
  result$latest_issue <- latest_issue
  
  message("[POLIO CORE] Latest issue detected:")
  message("  issue_id   : ", latest_issue$issue_id)
  message("  issue_date : ", latest_issue$issue_date)
  message("  source_url : ", latest_issue$source_url)
  message("  extracted  : ", latest_issue$extracted_at)
  
  # ------------------------------------------------------------
  # 3. Sauvegarder l'issue courante AVANT préparation
  #    Comme ça les scripts anciens qui lisent polio_last_issue.csv
  #    utilisent la nouvelle issue, pas l'ancienne.
  # ------------------------------------------------------------
  last_issue_file <- file.path(
    ROOT, "dashboard", "data", "dashboard", "polio_last_issue.csv"
  )
  
  dir.create(dirname(last_issue_file), recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(
    data.frame(
      issue_id = latest_issue$issue_id,
      issue_date = latest_issue$issue_date,
      source_url = latest_issue$source_url,
      extracted_at = latest_issue$extracted_at,
      stringsAsFactors = FALSE
    ),
    last_issue_file
  )
  
  message("[POLIO CORE] Current issue saved before rebuild: ", last_issue_file)
  
  # ------------------------------------------------------------
  # 4. Reconstruire les inputs alert/dashboard/email
  #    Appel compatible avec ancienne ou nouvelle signature.
  # ------------------------------------------------------------
  message("[POLIO CORE] STEP 2 — Rebuild alert/email inputs")
  
  prep_formals <- names(formals(prepare_polio_alert_input))
  
  result$prepare_output <- tryCatch({
    
    if ("project_dir" %in% prep_formals &&
        "latest_issue" %in% prep_formals &&
        "force_refresh" %in% prep_formals) {
      
      prepare_polio_alert_input(
        project_dir = ROOT,
        latest_issue = latest_issue,
        force_refresh = TRUE
      )
      
    } else if ("project_dir" %in% prep_formals &&
               "latest_issue" %in% prep_formals) {
      
      prepare_polio_alert_input(
        project_dir = ROOT,
        latest_issue = latest_issue
      )
      
    } else if ("project_dir" %in% prep_formals) {
      
      prepare_polio_alert_input(
        project_dir = ROOT
      )
      
    } else {
      
      prepare_polio_alert_input()
    }
    
  }, error = function(e) {
    stop("[POLIO CORE] prepare_polio_alert_input() failed: ", e$message)
  })
  
  result$prepared <- TRUE
  
  # ------------------------------------------------------------
  # 5. Re-sauvegarder l'issue courante après préparation
  #    Sécurité si prepare_polio_alert_input() réécrit l'ancien fichier.
  # ------------------------------------------------------------
  readr::write_csv(
    data.frame(
      issue_id = latest_issue$issue_id,
      issue_date = latest_issue$issue_date,
      source_url = latest_issue$source_url,
      extracted_at = latest_issue$extracted_at,
      stringsAsFactors = FALSE
    ),
    last_issue_file
  )
  
  message("[POLIO CORE] Current issue confirmed after rebuild: ", last_issue_file)
  
  # ------------------------------------------------------------
  # 6. Envoyer emails conditionnels RCC/HQ
  # ------------------------------------------------------------
  message("[POLIO CORE] STEP 3 — Conditional RCC/HQ emails")
  
  result$send_output <- tryCatch({
    
    send_polio_rcc_emails_conditional(
      send_now = send_now,
      force_send = force_send,
      project_dir = ROOT,
      dashboard_url = dashboard_url
    )
    
  }, error = function(e) {
    stop("[POLIO CORE] send_polio_rcc_emails_conditional() failed: ", e$message)
  })
  
  result$sent <- TRUE
  result$status <- "SUCCESS"
  
  message("[POLIO CORE] PIPELINE COMPLETE — SUCCESS")
  
  return(result)
}