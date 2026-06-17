# =========================================================
# R/100_run_polio_pipeline_if_update.R
# Orchestrateur principal polio
# =========================================================

run_polio_pipeline_if_update <- function(
    root = ".",
    send_now = TRUE,
    force_send = FALSE,
    save_issue_after_success = TRUE
) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd(root)
  
  message("=== POLIO UPDATE CHECK START ===")
  
  scripts_to_load <- c(
    "R/00_load_polio_project.R",
    "R/03_issue_registry.R",
    "R/02_check_polio_update.R",
    "R/04_run_polio_pipeline_core.R",
    "R/05_prepare_polio_alert_input.R",
    "R/09_send_polio_rcc_emails.R",
    "R/60_email.R"
  )
  
  for (f in scripts_to_load) {
    if (file.exists(f)) {
      source(f)
      message("Loaded: ", f)
    }
  }
  
  chk <- tryCatch(
    check_polio_update(root = "."),
    error = function(e) {
      message("Update check failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(chk)) {
    message("NO NEW POLIO UPDATE - pipeline not triggered")
    return(invisible(FALSE))
  }
  
  if (!isTRUE(chk$is_new) && !isTRUE(force_send)) {
    message("NO NEW POLIO UPDATE")
    message("Current issue unchanged: ", chk$current_issue)
    message("NO NEW POLIO UPDATE - pipeline not triggered")
    return(invisible(FALSE))
  }
  
  if (!isTRUE(chk$is_new) && isTRUE(force_send)) {
    message("NO NEW POLIO UPDATE")
    message("Current issue unchanged: ", chk$current_issue)
    message("FORCE_SEND = TRUE -> running pipeline anyway")
  } else {
    message("NEW UPDATE CONFIRMED: ", chk$current_issue)
  }
  
  if (is.null(chk)) {
    message("NO NEW POLIO UPDATE - pipeline not triggered")
    return(invisible(FALSE))
  }
  
  if (!isTRUE(chk$is_new) && isTRUE(force_send)) {
    message("NO NEW POLIO UPDATE")
    message("Current issue unchanged: ", chk$current_issue)
    message("FORCE_SEND = TRUE -> running pipeline anyway")
  } else if (!isTRUE(chk$is_new)) {
    message("NO NEW POLIO UPDATE - pipeline not triggered")
    return(invisible(FALSE))
  } else {
    message("NEW UPDATE CONFIRMED: ", chk$current_issue)
  }
  
  message("NEW UPDATE CONFIRMED: ", chk$current_issue)
  message("RUNNING POLIO PIPELINE...")
  
  pipe_res <- tryCatch(
    run_polio_pipeline_core(
      send_now = send_now,
      force_send = force_send
    ),
    error = function(e) {
      message("Pipeline failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(pipe_res)) {
    message("PIPELINE FAILED - last_issue not updated")
    return(invisible(FALSE))
  }
  
  if (isTRUE(save_issue_after_success) &&
      !is.null(chk$current_issue) &&
      nzchar(chk$current_issue)) {
    write_last_issue(chk$current_issue, root = ".")
    message("last_issue updated to: ", chk$current_issue)
  }
  
  message("=== POLIO PIPELINE COMPLETED ===")
  invisible(TRUE)
}