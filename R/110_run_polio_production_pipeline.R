# ============================================================
# R/110_run_polio_production_pipeline.R
# PREIS POLIO — Production autonomous runner
# ============================================================

ROOT <- "D:/PREIS_Polio_FV"
DASHBOARD_URL <- "https://zrhyacinthepreis26.shinyapps.io/dashboard/"

setwd(ROOT)

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "|", ..., "\n")
}

status <- "STARTED"

tryCatch({
  
  log_msg("PROJECT DIR:", ROOT)
  
  # 1. Charger le core
  log_msg("STEP 0 - LOAD CORE PIPELINE")
  source(file.path(ROOT, "R", "04_run_polio_pipeline_core.R"))
  
  # 2. Reconstruire données + email
  log_msg("STEP 1 - RUN CORE PIPELINE: latest GPEI data + conditional emails")
  
  res <- run_polio_pipeline_core(
    send_now = TRUE,
    force_send = FALSE,
    project_dir = ROOT,
    dashboard_url = DASHBOARD_URL
  )
  
  if (!identical(res$status, "SUCCESS")) {
    stop("Core pipeline did not return SUCCESS.")
  }
  
  log_msg("CORE PIPELINE SUCCESS")
  log_msg("Latest issue:", res$latest_issue$issue_id)
  log_msg("Issue date:", res$latest_issue$issue_date)
  
  # 3. Déployer dashboard après mise à jour des CSV
  log_msg("STEP 2 - DEPLOY DASHBOARD")
  
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package rsconnect is not installed.")
  }
  
  rsconnect::deployApp(
    appDir = file.path(ROOT, "dashboard"),
    appName = "dashboard",
    forceUpdate = TRUE
  )
  
  log_msg("DASHBOARD DEPLOYED OK")
  
  status <- "SUCCESS"
  
}, error = function(e) {
  
  status <<- "FAILED"
  log_msg("PIPELINE FAILED:", e$message)
  
})

log_msg("FINAL STATUS:", status)

if (status != "SUCCESS") {
  stop("PREIS POLIO production pipeline failed.")
}