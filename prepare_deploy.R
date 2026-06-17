# ============================================================
# prepare_deploy.R
# Prépare le dossier dashboard/ pour shinyapps.io
# Copie config/ et data/ à l'intérieur de dashboard/
# puis redéploie
#
# Exécuter : source("D:/PREIS_Polio_FV/prepare_deploy.R")
# ============================================================

ROOT <- "D:/PREIS_Polio_FV"
DASH <- file.path(ROOT, "dashboard")

cat("Préparation du déploiement shinyapps.io...\n\n")

# ---- 1. Copier config/ dans dashboard/config/ ----
dir.create(file.path(DASH, "config"), showWarnings = FALSE)

config_files <- c(
  "rcc_country_fv.csv",
  "africa_gazetteer.csv",
  "country_coords.csv",
  "pathogen_dictionary_master.csv"
)

cat("Config files :\n")
for (f in config_files) {
  src  <- file.path(ROOT, "config", f)
  dest <- file.path(DASH, "config", f)
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    cat("  ✓", f, "\n")
  } else {
    cat("  ✗", f, "(manquant)\n")
  }
}

# ---- 2. Copier data/curated/ dans dashboard/data/curated/ ----
dir.create(file.path(DASH, "data", "curated"), recursive = TRUE, showWarnings = FALSE)

cat("\nData curated :\n")
src  <- file.path(ROOT, "data", "curated", "africa_countries_rcc.geojson")
dest <- file.path(DASH, "data", "curated", "africa_countries_rcc.geojson")
if (file.exists(src)) {
  file.copy(src, dest, overwrite = TRUE)
  cat("  ✓ africa_countries_rcc.geojson\n")
} else {
  cat("  ✗ africa_countries_rcc.geojson (manquant)\n")
}

# ---- 3. S'assurer que dashboard/data/dashboard/ existe avec les CSV ----
dir.create(file.path(DASH, "data", "dashboard"), recursive = TRUE, showWarnings = FALSE)

data_files <- c(
  "polio_africa_email_input.csv",
  "polio_alert_input.csv",
  "polio_last_issue.csv",
  "alert_recipients.csv"
)

cat("\nDashboard data :\n")
for (f in data_files) {
  fp <- file.path(DASH, "data", "dashboard", f)
  if (file.exists(fp)) {
    cat("  ✓", f, "\n")
  } else {
    # Créer un fichier vide pour que l'app démarre
    readr::write_csv(tibble::tibble(), fp)
    cat("  ✓", f, "(créé vide)\n")
  }
}

# ---- 4. Patcher app.R pour chemins relatifs ----
cat("\nPatch app.R — chemins relatifs...\n")

app_file <- file.path(DASH, "app.R")
app_txt  <- readLines(app_file, encoding = "UTF-8")

# Remplacer le bloc ROOT_DIR par une version relative
old_block <- 'ROOT_DIR <- normalizePath("D:/PREIS_Polio_FV", winslash = "/", mustWork = FALSE)'
new_block <- paste0(
  '# Chemins relatifs pour shinyapps.io\n',
  'ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)\n',
  '# Sur shinyapps.io, getwd() = /srv/connect/apps/dashboard\n',
  '# config/, data/ et le geojson sont copiés dans dashboard/'
)

# Remplacer aussi les lignes qui remontent d'un niveau (..)
app_txt <- gsub(
  'ROOT_DIR <- normalizePath\\("D:/PREIS_Polio_FV".*\\)',
  'ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)',
  app_txt
)

# Supprimer le bloc fallback qui remonte vers ".."
app_txt <- gsub(
  'if \\(!dir\\.exists\\(ROOT_DIR\\)\\) \\{.*?\\}',
  '',
  paste(app_txt, collapse = "\n")
)

# Réécrire
writeLines(app_txt, app_file, useBytes = FALSE)
cat("  ✓ app.R patché\n")

# ---- 5. Vérification finale ----
cat("\nStructure dashboard/ :\n")
all_files <- list.files(DASH, recursive = TRUE, full.names = FALSE)
for (f in sort(all_files)) {
  if (!grepl("before_|backup_|_tmp", f))  # masquer les backups
    cat("  ", f, "\n")
}

# ---- 6. Redéployer ----
cat("\nRedéploiement sur shinyapps.io...\n")
rsconnect::deployApp(
  appDir  = DASH,
  appName = "dashboard",
  forceUpdate = TRUE
)
