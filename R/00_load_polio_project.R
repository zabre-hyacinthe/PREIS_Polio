# =========================================================
# R/00_load_polio_project.R
# PREIS_Polio_FV -- Chargement de tous les modules
# =========================================================

safe_source <- function(fp) {
  if (!file.exists(fp)) stop("Missing script: ", fp, call. = FALSE)
  source(fp)
  cat("Loaded:", fp, "\n")
}

safe_source("R/03_issue_registry.R")
safe_source("R/02_check_polio_update.R")
safe_source("R/05_prepare_polio_alert_input.R")
safe_source("R/60_email.R")
safe_source("R/09_send_polio_rcc_emails.R")
safe_source("R/04_run_polio_pipeline_core.R")
safe_source("R/100_run_polio_pipeline_if_update.R")

# Verification des fonctions critiques
required <- c(
  "read_last_issue", "write_last_issue", "check_polio_update",
  "prepare_polio_alert_input", "send_email_safely",
  "send_polio_rcc_emails_conditional", "run_polio_pipeline_core",
  "run_polio_pipeline_if_update"
)
missing <- required[!vapply(required, exists, logical(1), mode = "function")]
if (length(missing) > 0) {
  stop("Fonctions manquantes apres chargement: ", paste(missing, collapse = ", "))
}
cat("Tous les modules PREIS_Polio_FV charges.\n")
