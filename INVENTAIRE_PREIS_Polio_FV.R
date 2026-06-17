# ============================================================
# INVENTAIRE_PREIS_Polio_FV.R
# Audit complet du projet PREIS_Polio_FV
# setwd("D:/PREIS_Polio_FV") puis source("INVENTAIRE_PREIS_Polio_FV.R")
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("\n====================================\n")
cat("INVENTAIRE PREIS_Polio_FV\n")
cat("====================================\n")
cat("Projet :", ROOT, "\n")
cat("Date   :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# -- 1. COLLECTE FICHIERS -------------------------------------

all_files <- list.files(ROOT, recursive = TRUE, full.names = TRUE, all.files = TRUE)
all_files <- all_files[!str_detect(all_files, "[/\\\\][.]git[/\\\\]")]
all_files <- normalizePath(all_files, winslash = "/")
rel_files <- str_remove(all_files, paste0(ROOT, "/"))

classify_file <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  base <- basename(path)
  case_when(
    ext == "r"                                        ~ "Script R",
    ext %in% c("rmd", "qmd")                         ~ "R Markdown",
    ext %in% c("bat", "cmd", "ps1", "sh")            ~ "Lanceur / automation",
    ext == "csv"                                      ~ "Donnees CSV",
    ext %in% c("xlsx", "xls")                        ~ "Donnees Excel",
    ext == "txt"                                      ~ "Texte",
    ext == "log"                                      ~ "Log",
    ext %in% c("json", "yaml", "yml", "toml")        ~ "Config",
    base %in% c(".Renviron", ".Rprofile")             ~ "Config R env",
    str_detect(base, "\\.bak")                        ~ "Backup",
    ext %in% c("md", "html", "pdf", "docx")          ~ "Documentation",
    ext == "geojson"                                  ~ "GeoJSON",
    ext == "dcf"                                      ~ "Autre (.dcf)",
    TRUE                                              ~ paste0("Autre (.", ext, ")")
  )
}

inv <- tibble(
  fichier = rel_files,
  chemin  = all_files,
  type    = classify_file(rel_files),
  taille  = file.size(all_files),
  modifie = format(file.mtime(all_files), "%Y-%m-%d %H:%M")
)

# -- 2. AFFICHAGE PAR TYPE ------------------------------------

cat("--- FICHIERS PAR TYPE ---\n\n")
for (cat_type in sort(unique(inv$type))) {
  sub <- inv %>% filter(type == cat_type)
  cat(sprintf("[%s] - %d fichier(s)\n", cat_type, nrow(sub)))
  for (i in seq_len(nrow(sub))) {
    kb <- round(sub$taille[i] / 1024, 1)
    cat(sprintf("  %-55s  %6.1f KB  %s\n", sub$fichier[i], kb, sub$modifie[i]))
  }
  cat("\n")
}
cat(sprintf("TOTAL : %d fichiers\n\n", nrow(inv)))

# -- 3. AUDIT NOMENCLATURE ------------------------------------

cat("--- AUDIT NOMENCLATURE ---\n\n")

# Patterns construits dynamiquement (evite les faux positifs sur ce script lui-meme)
p_maj  <- paste0("PREIS", "_POLIO")
p_esp  <- paste0("PREIS", " POLIO")
p_min  <- paste0("preis", "_polio(?!", "_fv)")
p_path <- paste0("D:/PREIS", "_POLIO")

patterns_incorrects <- list(
  "ancien_nom_majuscules" = p_maj,
  "ancien_nom_espace"     = p_esp,
  "ancien_nom_minuscules" = p_min,
  "ancien_chemin_D"       = p_path
)

text_exts <- c("r", "rmd", "qmd", "bat", "cmd", "ps1", "sh",
                "csv", "txt", "log", "yaml", "yml", "json", "toml", "")

readable <- inv %>%
  filter(
    tolower(tools::file_ext(fichier)) %in% text_exts,
    type != "Backup",
    !str_detect(fichier, "^ARCHIVE_UNUSED/"),
    !str_detect(fichier, "^outputs/logs/audit_"),
    !str_detect(fichier, "^outputs/logs/inventaire_"),
    !str_detect(fichier, "INVENTAIRE_PREIS_Polio_FV[.]R"),
    !str_detect(fichier, "^PATCH_"),
    !str_detect(fichier, "[.]md$"),
    taille < 2e6
  )

audit_rows <- list()

for (i in seq_len(nrow(readable))) {
  fp  <- readable$chemin[i]
  rel <- readable$fichier[i]
  lines <- tryCatch(
    readLines(fp, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
  if (length(lines) == 0L) next

  for (pat_name in names(patterns_incorrects)) {
    pat  <- patterns_incorrects[[pat_name]]
    hits <- which(str_detect(lines, regex(pat, ignore_case = FALSE)))
    for (ln in hits) {
      audit_rows[[length(audit_rows) + 1L]] <- tibble(
        fichier  = rel,
        ligne    = ln,
        probleme = pat_name,
        contenu  = str_squish(str_trunc(lines[ln], 90))
      )
    }
  }
}

audit <- bind_rows(audit_rows)

if (nrow(audit) == 0L) {
  cat("OK - Aucun probleme de nomenclature detecte.\n\n")
} else {
  cat(sprintf("ATTENTION : %d occurrence(s) a corriger :\n\n", nrow(audit)))
  for (f in unique(audit$fichier)) {
    sub_a <- audit %>% filter(fichier == f)
    cat(sprintf("  Fichier : %s\n", f))
    for (j in seq_len(nrow(sub_a))) {
      cat(sprintf("    L.%d [%s]\n      %s\n",
                  sub_a$ligne[j], sub_a$probleme[j], sub_a$contenu[j]))
    }
    cat("\n")
  }
}

# -- 4. FONCTIONS CRITIQUES -----------------------------------

cat("--- FONCTIONS CRITIQUES ---\n\n")

fns_critiques <- c(
  "read_last_issue", "write_last_issue", "normalize_issue_id",
  "check_polio_update", "prepare_polio_alert_input",
  "send_email_safely", "preis_send_email",
  "send_polio_rcc_emails_conditional", "build_polio_email_body",
  "run_polio_pipeline_core", "run_polio_pipeline_if_update"
)

r_files <- inv %>% filter(type == "Script R")

fn_found <- tibble(
  fonction = fns_critiques,
  fichier  = NA_character_
)

for (fn in fns_critiques) {
  pat <- paste0("^\\s*", fn, "\\s*(<-|=)\\s*function")
  for (i in seq_len(nrow(r_files))) {
    lines <- tryCatch(
      readLines(r_files$chemin[i], warn = FALSE, encoding = "UTF-8"),
      error = function(e) character(0)
    )
    if (length(lines) == 0L) next
    if (any(str_detect(lines, regex(pat)))) {
      fn_found$fichier[fn_found$fonction == fn] <- r_files$fichier[i]
      break
    }
  }
}

for (i in seq_len(nrow(fn_found))) {
  found <- !is.na(fn_found$fichier[i])
  cat(sprintf("  [%s] %-45s -> %s\n",
              if (found) "OK" else "!!",
              fn_found$fonction[i],
              if (found) fn_found$fichier[i] else "INTROUVABLE"))
}
cat("\n")

# -- 5. FICHIERS OBLIGATOIRES ---------------------------------

cat("--- FICHIERS OBLIGATOIRES ---\n\n")

requis <- list(
  "Scripts R" = c(
    "R/00_load_polio_project.R", "R/02_check_polio_update.R",
    "R/03_issue_registry.R",     "R/04_run_polio_pipeline_core.R",
    "R/05_prepare_polio_alert_input.R", "R/09_send_polio_rcc_emails.R",
    "R/60_email.R",              "R/100_run_polio_pipeline_if_update.R"
  ),
  "Production" = c("R/110_run_polio_production_pipeline.R"),
  "Automation" = c("run_polio_pipeline.bat"),
  "Config"     = c(".Renviron", "config/rcc_country_fv.csv"),
  "Donnees"    = c("dashboard/data/dashboard/alert_recipients.csv"),
  "Registre"   = c("data/last_issue.txt")
)

for (cat_name in names(requis)) {
  cat(sprintf("  [%s]\n", cat_name))
  for (f in requis[[cat_name]]) {
    ok <- file.exists(file.path(ROOT, f))
    cat(sprintf("    [%s] %s\n", if (ok) "OK" else "MANQUANT", f))
  }
  cat("\n")
}

# -- 6. ORPHELINS ---------------------------------------------

cat("--- SCRIPTS ORPHELINS ---\n\n")

scripts_prod <- c(
  "R/00_load_polio_project.R",   "R/02_check_polio_update.R",
  "R/03_issue_registry.R",       "R/04_run_polio_pipeline_core.R",
  "R/05_prepare_polio_alert_input.R", "R/09_send_polio_rcc_emails.R",
  "R/60_email.R",                "R/100_run_polio_pipeline_if_update.R",
  "R/110_run_polio_production_pipeline.R",
  "run_polio_pipeline.bat",      "INVENTAIRE_PREIS_Polio_FV.R",
  "DIAGNOSTIC_run_me_first.R",   "PATCH_app_nomenclature.R",
  "900_archive_cleanup.R",       "R/900_archive_cleanup.R",
  "deploy_final.R",              "prepare_deploy.R",
  "dashboard/app.R",             "PREIS_Polio_FV_Documentation.md",
  "data/last_issue.txt",         "R/63_send_alerts_conditional.R"
)

orphelins <- inv %>%
  filter(
    type %in% c("Script R", "Lanceur / automation", "Backup"),
    !fichier %in% scripts_prod,
    !str_detect(fichier, "^ARCHIVE_UNUSED/")
  )

if (nrow(orphelins) == 0L) {
  cat("OK - Pas de scripts orphelins.\n\n")
} else {
  cat(sprintf("%d script(s) hors pipeline :\n\n", nrow(orphelins)))
  for (i in seq_len(nrow(orphelins))) {
    cat(sprintf("  [%-20s] %s\n", orphelins$type[i], orphelins$fichier[i]))
  }
  cat("\n  -> A archiver dans ARCHIVE_UNUSED/\n\n")
}

# -- 7. EXPORT CSV --------------------------------------------

rapport_dir <- file.path(ROOT, "outputs", "logs")
dir.create(rapport_dir, recursive = TRUE, showWarnings = FALSE)

rapport_fp <- file.path(rapport_dir,
  paste0("inventaire_", format(Sys.Date(), "%Y%m%d"), ".csv"))

rapport <- inv %>%
  mutate(
    taille_kb       = round(taille / 1024, 1),
    nomenclature_ok = !fichier %in% unique(audit$fichier)
  ) %>%
  select(fichier, type, taille_kb, modifie, nomenclature_ok)

write_csv(rapport, rapport_fp)

if (nrow(audit) > 0L) {
  audit_fp <- file.path(rapport_dir,
    paste0("audit_nomenclature_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write_csv(audit, audit_fp)
  cat(sprintf("Rapport audit    : %s\n", audit_fp))
}

# -- 8. RESUME FINAL ------------------------------------------

cat("====================================\n")
cat("RESUME FINAL\n")
cat("====================================\n\n")
cat(sprintf("  Fichiers totaux         : %d\n", nrow(inv)))
cat(sprintf("  Scripts R               : %d\n", sum(inv$type == "Script R")))
cat(sprintf("  Backups                 : %d\n", sum(inv$type == "Backup")))
cat(sprintf("  Problemes nomenclature  : %d occurrence(s)\n", nrow(audit)))
cat(sprintf("  Fonctions manquantes    : %d\n",  sum(is.na(fn_found$fichier))))
cat(sprintf("  Orphelins a archiver    : %d\n",  nrow(orphelins)))
cat(sprintf("\nRapport inventaire : %s\n", rapport_fp))
cat("\n====================================\n")
cat("INVENTAIRE TERMINE\n")
cat("====================================\n\n")
