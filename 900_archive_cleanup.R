# ============================================================
# 900_archive_cleanup.R
# PREIS_Polio_FV — Archivage des fichiers hors production
# ============================================================
# Lance depuis la racine du projet :
#   setwd("D:/PREIS_Polio_FV")
#   source("900_archive_cleanup.R")
#
# Ce script déplace (pas supprime) les 14 fichiers orphelins
# vers ARCHIVE_UNUSED/ avec un horodatage.
# ============================================================

ROOT    <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
ARCHIVE <- file.path(ROOT, "ARCHIVE_UNUSED", paste0("archive_", Sys.Date()))

dir.create(ARCHIVE, recursive = TRUE, showWarnings = FALSE)

cat("\n====================================\n")
cat("PREIS_Polio_FV — ARCHIVAGE CLEANUP\n")
cat("====================================\n")
cat("Racine  :", ROOT, "\n")
cat("Archive :", ARCHIVE, "\n\n")

# ── Liste des fichiers à archiver ───────────────────────────
# Fichiers à la RACINE du projet (pas dans R/)
to_archive_root <- c(
  "00_load_polio_project_CORRECTED.R",
  "05_prepare_polio_alert_input_CORRECTED.R",
  "100_run_polio_pipeline_if_update_CORRECTED.R",
  "110_run_polio_production_pipeline_CORRECTED.R",
  "60_email_CORRECTED.R",
  "63_send_alerts_conditional_CORRECTED.R",
  "900_project_cleanup_CORRECTED_POLIO.R",
  "app_CORRECTED.R",
  "fix_missing_scripts.R",
  "create_missing_scripts.R",
  "build_PREIS_Polio_FV.R",
  "patch_05.R",
  "patch_05_v2.R",
  "patch_09b.R",
  "patch_110.R",
  "patch_110_autodeploy.R"
)

# Fichiers dans R/ qui sont des doublons ou tests
to_archive_R <- c(
  "R/09b_send_polio_rcc_emails_conditional.R",
  "R/100_run_polio_weekly_full_pipeline.R",
  "R/TEST_01_fetch_gpei.R",
  "R/TEST_01_fetch_gpei_v2.R",
  "R/TEST_02_full_pipeline.R",
  "R/05_prepare_polio_alert_input_FINAL.R"
)

all_to_archive <- c(to_archive_root, to_archive_R)

# ── Déplacement ─────────────────────────────────────────────
moved   <- 0
skipped <- 0

for (f in all_to_archive) {
  src <- file.path(ROOT, f)

  if (!file.exists(src)) {
    cat(sprintf("  [SKIP]    %s (introuvable)\n", f))
    skipped <- skipped + 1
    next
  }

  # Destination : on préserve le sous-dossier (R/ ou racine)
  dest_name <- gsub("/", "_", f)  # R/09b_... -> R_09b_...
  dest <- file.path(ARCHIVE, dest_name)

  ok <- file.rename(src, dest)

  if (ok) {
    cat(sprintf("  [ARCHIVÉ] %s\n", f))
    moved <- moved + 1
  } else {
    cat(sprintf("  [ERREUR]  %s — rename échoué\n", f))
  }
}

cat("\n")
cat(sprintf("Archivés : %d fichiers\n", moved))
cat(sprintf("Ignorés  : %d fichiers (déjà absents)\n", skipped))
cat(sprintf("Dossier  : %s\n", ARCHIVE))
cat("\n====================================\n")
cat("CLEANUP TERMINÉ\n")
cat("====================================\n\n")
cat("Prochaine étape : relancer INVENTAIRE_PREIS_Polio_FV.R\n")
cat("pour confirmer que le projet est propre.\n\n")
