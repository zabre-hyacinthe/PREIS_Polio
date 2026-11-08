# ============================================================
# R/04_run_polio_pipeline_core.R — PREIS POLIO Core Pipeline
# Version robuste sans casser les anciens scripts
# v2 (2026-11-03) : le registre last_issue.txt n'avance plus en mode test ni
# si aucun envoi n'a reussi (voir etape 7).
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
  # GARDE-FOU 06/11/2026 : edition de la page plus ancienne que la derniere traitee
  # -> aucune ecriture (registre, dashboard), aucun envoi (sauf force_send).
  if (isTRUE(check_result$is_stale) && !isTRUE(force_send)) {
    stale_msg <- paste0(
      "la page GPEI indique ", latest_issue$issue_date,
      ", plus ancien que la derniere edition traitee (", check_result$last_issue,
      ") : aucune ecriture, aucun envoi."
    )
    message("[POLIO CORE] STOP -- ", stale_msg)
    cat("::warning title=PREIS Polio source older than last processed edition::", stale_msg, "\n", sep = "")
    result$status <- "SUCCESS_NO_UPDATE"
    result$stale <- TRUE
    return(result)
  }
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
  #
  # CORRECTIF 09/08/2026 -- 3e bug trouve dans la meme investigation que
  # les deux precedents (date fausse + registre fige). Meme une fois ces
  # deux-la corriges, RIEN dans ce script n'empechait l'envoi d'emails
  # quand check_polio_update() rapportait "NO NEW POLIO UPDATE" -- le
  # pipeline continuait quand meme jusqu'a l'envoi, chaque jour, tant que
  # GPEI ne publie pas une nouvelle edition. "conditional" dans le nom du
  # script ne voulait dire que "si cette RCC a des alertes cette
  # edition", jamais "si cette edition est nouvelle depuis le dernier
  # envoi reussi".
  #
  # force_send existait deja comme parametre mais n'etait jamais utilise
  # pour ce role : on l'utilise maintenant comme prevu, pour permettre un
  # renvoi manuel volontaire (tests, ou renvoi apres un incident) sans
  # toucher au comportement automatique quotidien.
  # ------------------------------------------------------------
  if (isTRUE(check_result$is_new) || isTRUE(force_send)) {

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

  } else {

    message(
      "[POLIO CORE] STEP 3 SKIPPED -- edition inchangee depuis le dernier ",
      "envoi reussi (is_new=FALSE) et force_send=FALSE. Aucun email envoye ",
      "aujourd'hui. Utiliser force_send=TRUE (workflow_dispatch) pour forcer ",
      "un renvoi manuel si necessaire."
    )
    result$send_output <- NULL
    result$sent <- FALSE
  }

  # ------------------------------------------------------------
  # 7. Marquer cette edition comme traitee (CORRECTIF 08/08/2026)
  #    BUG TROUVE EN PRODUCTION : write_last_issue() n'etait appele nulle
  #    part dans tout le pipeline. data/last_issue.txt restait donc fige
  #    (bloque au 22 avril 2026) alors que le workflow tourne chaque jour
  #    (cron 0 5 * * *) et que GPEI ne publie qu'une fois par semaine.
  #    Consequence reelle : check_polio_update() comparait TOUJOURS au
  #    22 avril, detectait "une nouvelle edition" a CHAQUE run quotidien,
  #    et renvoyait les emails RCC chaque jour meme quand rien n'avait
  #    change depuis la veille.
  #    Ecrit seulement ICI, apres succes complet (reconstruction ET
  #    envoi des emails) : une edition n'est marquee traitee que si tout
  #    le pipeline a reellement reussi, jamais avant.
  # ------------------------------------------------------------
  # CORRECTIF 03/11/2026 (v2) : le registre n'est avance que si l'edition a
  # VRAIMENT ete traitee. Avant, write_last_issue() etait appele meme en mode
  # test (PREIS_DRY_RUN=true, defaut du workflow_dispatch) et meme si tous les
  # envois avaient echoue ou ete bloques : l'edition etait alors marquee
  # "deja traitee" et le cron suivant n'envoyait plus jamais l'alerte reelle.
  # Regles :
  #   - mode test (dry-run) -> registre NON avance ;
  #   - aucun envoi effectue et au moins un echec -> registre NON avance
  #     (nouvelle tentative au prochain passage) ;
  #   - au moins un envoi reussi -> registre avance (echecs partiels signales) ;
  #   - aucun destinataire concerne (tout "skipped_no_alert") -> registre avance.
  is_dry <- if (exists("is_dry_run_email", mode = "function")) {
    isTRUE(is_dry_run_email(send_now = send_now))
  } else {
    !isTRUE(send_now) ||
      tolower(Sys.getenv("PREIS_DRY_RUN", "false")) %in% c("1", "true", "yes", "y")
  }
  so <- result$send_output
  st <- if (is.data.frame(so) && "status" %in% names(so)) as.character(so$status) else character(0)
  n_sent   <- sum(st == "sent")
  n_failed <- sum(grepl("^(failed|blocked)", st))

  mark_processed <- FALSE
  if (is.null(so)) {
    mark_processed <- TRUE   # edition inchangee ou rien a envoyer : rien a rejouer
  } else if (is_dry) {
    message("[POLIO CORE] Mode test (dry-run) : registre last_issue.txt NON avance, ",
            "l'edition reste 'nouvelle' pour le prochain envoi reel.")
  } else if (n_sent > 0) {
    mark_processed <- TRUE
    if (n_failed > 0) {
      warning("[POLIO CORE] ", n_failed, " envoi(s) en echec/bloques sur ", length(st),
              " ; registre avance car ", n_sent, " envoi(s) ont reussi.", call. = FALSE)
    }
  } else if (n_failed > 0) {
    message("[POLIO CORE] Aucun envoi reussi et ", n_failed, " echec(s)/blocage(s) : ",
            "registre last_issue.txt NON avance (nouvelle tentative au prochain passage).")
    # Visible dans l'interface GitHub Actions sans bloquer le commit des donnees
    # du tableau de bord (le job reste vert, une annotation d'avertissement apparait).
    cat("::warning title=PREIS Polio alerts NOT sent::", n_failed,
        " envoi(s) en echec/bloques, aucun reussi ; edition ", latest_issue$issue_id,
        " non marquee traitee (nouvelle tentative au prochain passage).\n", sep = "")
  } else {
    mark_processed <- TRUE   # tout etait "skipped_no_alert"
  }

  if (mark_processed) {
    tryCatch({
      write_last_issue(latest_issue$issue_id, root = ROOT)
      message("[POLIO CORE] Registre last_issue.txt mis a jour -> ", latest_issue$issue_id)
    }, error = function(e) {
      warning(
        "[POLIO CORE] Echec de la mise a jour de last_issue.txt : ", e$message,
        call. = FALSE
      )
    })
  }

  result$status <- if (isTRUE(result$sent)) "SUCCESS" else "SUCCESS_NO_UPDATE"

  message("[POLIO CORE] PIPELINE COMPLETE — ", result$status)
  
  return(result)
}