# ============================================================
# R/120_run_polio_cloud.R
# PREIS POLIO — Runner CLOUD (GitHub Actions)
# ============================================================
# Difference avec 110_run_polio_production_pipeline.R :
#   - PAS de rsconnect::deployApp (le dashboard lit GitHub directement)
#   - Lance l'adapter pour mettre a jour le socle commun (preis_common)
#   - Concu pour tourner sur ubuntu-latest (chemins relatifs)
#
# Etapes :
#   0. Charger le core pipeline
#   1. Verifier nouvelle issue GPEI + traiter + emails conditionnels
#   2. Lancer l'adapter -> data/final/preis_common/ (socle commun)
#   3. Le workflow GitHub Actions commit/push les CSV ensuite
# ============================================================

# Sur GitHub Actions, le repo est checkout dans le working directory.
ROOT <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DASHBOARD_URL <- "https://zrhyacinthepreis26.shinyapps.io/dashboard/"

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "|", ..., "\n")
}

status <- "STARTED"

tryCatch({
  
  log_msg("PROJECT DIR:", ROOT)
  log_msg("Dry run mode:", Sys.getenv("PREIS_DRY_RUN", "false"))
  
  # 1. Charger le core
  log_msg("STEP 0 - LOAD CORE PIPELINE")
  source(file.path(ROOT, "R", "04_run_polio_pipeline_core.R"))
  
  # 2. Reconstruire donnees + emails conditionnels
  log_msg("STEP 1 - RUN CORE PIPELINE: latest GPEI data + conditional emails")
  
  res <- run_polio_pipeline_core(
    send_now      = TRUE,
    force_send    = FALSE,        # envois CONDITIONNELS preserves
    project_dir   = ROOT,
    dashboard_url = DASHBOARD_URL
  )
  
  if (!identical(res$status, "SUCCESS")) {
    stop("Core pipeline did not return SUCCESS.")
  }
  
  log_msg("CORE PIPELINE SUCCESS")
  if (!is.null(res$latest_issue)) {
    log_msg("Latest issue:", res$latest_issue$issue_id)
    log_msg("Issue date:", res$latest_issue$issue_date)
  }
  
  # 3. Mettre a jour le socle commun (adapter)
  log_msg("STEP 2 - RUN ADAPTER (socle commun PREIS)")
  
  adapter_fp <- file.path(ROOT, "00_preis_adapter_polio.R")
  if (file.exists(adapter_fp)) {
    Sys.setenv(PREIS_POLIO_ROOT = ROOT)
    source(adapter_fp)
    log_msg("ADAPTER OK - preis_common mis a jour")
  } else {
    log_msg("WARNING: adapter introuvable, socle commun non mis a jour")
  }
  
  status <- "SUCCESS"
  
}, error = function(e) {
  status <<- "FAILED"
  log_msg("PIPELINE FAILED:", conditionMessage(e))
})

log_msg("FINAL STATUS:", status)

if (status != "SUCCESS") {
  quit(status = 1)   # fait echouer le job GitHub Actions si erreur
}

# FIN : 120_run_polio_cloud.R
