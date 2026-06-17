# ============================================================
# NETTOYAGE_final.R
# PREIS_Polio_FV - Suppression des doublons racine
# ============================================================
# setwd("D:/PREIS_Polio_FV") puis source("NETTOYAGE_final.R")
# ============================================================

cat("====================================\n")
cat("NETTOYAGE FINAL PREIS_Polio_FV\n")
cat("====================================\n\n")

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
cat("Racine :", ROOT, "\n\n")

# -- 1. DOUBLONS A SUPPRIMER (copies a la racine, vraie version dans R/) --

doublons <- c(
  "09_send_polio_rcc_emails.R",
  "110_run_polio_production_pipeline.R",
  "60_email.R",
  "900_project_cleanup.R"
)

cat("--- Suppression des doublons racine ---\n")
for (f in doublons) {
  fp <- file.path(ROOT, f)
  if (file.exists(fp)) {
    file.remove(fp)
    cat(sprintf("  [SUPPRIME] %s\n", f))
  } else {
    cat(sprintf("  [ABSENT]   %s\n", f))
  }
}
cat("\n")

# -- 2. DEPLACEMENT R/900_archive_cleanup.R vers racine -------

src <- file.path(ROOT, "R", "900_archive_cleanup.R")
dst <- file.path(ROOT, "900_archive_cleanup.R")

cat("--- Deplacement R/900_archive_cleanup.R -> racine ---\n")
if (file.exists(src)) {
  if (file.exists(dst)) {
    cat("  [DEJA LA] 900_archive_cleanup.R existe deja a la racine\n")
    file.remove(src)
    cat("  [SUPPRIME] R/900_archive_cleanup.R (doublon)\n")
  } else {
    file.rename(src, dst)
    cat("  [DEPLACE] R/900_archive_cleanup.R -> racine\n")
  }
} else {
  cat("  [ABSENT] R/900_archive_cleanup.R introuvable\n")
}
cat("\n")

# -- 3. ARCHIVAGE R/63_send_alerts_conditional.R --------------

src63 <- file.path(ROOT, "R", "63_send_alerts_conditional.R")
archive_dir <- file.path(ROOT, "ARCHIVE_UNUSED",
                          paste0("archive_", Sys.Date()))

cat("--- Archivage R/63_send_alerts_conditional.R ---\n")
if (file.exists(src63)) {
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  dst63 <- file.path(archive_dir, "63_send_alerts_conditional.R")
  file.rename(src63, dst63)
  cat(sprintf("  [ARCHIVE] -> %s\n", dst63))
} else {
  cat("  [ABSENT] R/63_send_alerts_conditional.R introuvable\n")
}
cat("\n")

# -- 4. VERIFICATION FINALE -----------------------------------

cat("--- Verification ---\n")
checks <- c(
  "R/09_send_polio_rcc_emails.R",
  "R/60_email.R",
  "R/110_run_polio_production_pipeline.R",
  "R/100_run_polio_pipeline_if_update.R",
  "900_archive_cleanup.R",
  "run_polio_pipeline.bat"
)
for (f in checks) {
  ok <- file.exists(file.path(ROOT, f))
  cat(sprintf("  [%s] %s\n", if (ok) "OK" else "MANQUANT", f))
}

cat("\n====================================\n")
cat("NETTOYAGE TERMINE\n")
cat("====================================\n\n")
cat("Relance INVENTAIRE_PREIS_Polio_FV.R pour confirmer 0 orphelin.\n\n")
