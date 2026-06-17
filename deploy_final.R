# ============================================================
# deploy_final.R
# Reecrit app.R avec chemins relatifs puis deploie
# Executer depuis D:/PREIS_Polio_FV : source("deploy_final.R")
# ============================================================

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DASH <- file.path(ROOT, "dashboard")

cat("=== PREPARATION DEPLOIEMENT ===\n\n")

# ---- 1. Copier tous les fichiers necessaires dans dashboard/ ----
cat("1. Copie des fichiers de donnees...\n")

# config/
dir.create(file.path(DASH, "config"), showWarnings = FALSE)
for (f in list.files(file.path(ROOT, "config"), full.names = TRUE)) {
  file.copy(f, file.path(DASH, "config", basename(f)), overwrite = TRUE)
  cat("   [OK] config/", basename(f), "\n")
}

# data/curated/
dir.create(file.path(DASH, "data", "curated"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  file.path(ROOT, "data", "curated", "africa_countries_rcc.geojson"),
  file.path(DASH, "data", "curated", "africa_countries_rcc.geojson"),
  overwrite = TRUE
)
cat("   [OK] data/curated/africa_countries_rcc.geojson\n")

# data/dashboard/ - s'assurer que les CSV existent
dir.create(file.path(DASH, "data", "dashboard"), recursive = TRUE, showWarnings = FALSE)
for (f in c("polio_africa_email_input.csv", "polio_alert_input.csv",
            "polio_last_issue.csv", "alert_recipients.csv")) {
  fp <- file.path(DASH, "data", "dashboard", f)
  if (!file.exists(fp)) {
    readr::write_csv(tibble::tibble(), fp)
    cat("   [OK] data/dashboard/", f, "(cree vide)\n")
  } else {
    cat("   [OK] data/dashboard/", f, "\n")
  }
}

# ---- 2. Reecrire app.R avec chemins relatifs corrects ----
cat("\n2. Reecriture app.R avec chemins relatifs...\n")

# Lire le contenu actuel
app_lines <- readLines(file.path(DASH, "app.R"), encoding = "UTF-8", warn = FALSE)

# Trouver et remplacer le bloc de chemins
# ROOT_DIR est resolu dynamiquement via getwd()
# Sur shinyapps.io, getwd() = /srv/connect/apps/dashboard
# Donc config/ et data/ sont DIRECTEMENT dans getwd()

new_path_block <- c(
  '# ------------------------------------------------------------',
  '# PATHS - chemins relatifs pour shinyapps.io',
  '# Sur shinyapps.io : getwd() = /srv/connect/apps/dashboard',
  '# config/ et data/ sont copies dans dashboard/ avant deploiement',
  '# ------------------------------------------------------------',
  'ROOT_DIR      <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)',
  'DASH_DIR      <- ROOT_DIR',
  'DASH_DATA_DIR <- file.path(ROOT_DIR, "data", "dashboard")',
  'CFG_DIR       <- file.path(ROOT_DIR, "config")',
  'CURATED_DIR   <- file.path(ROOT_DIR, "data", "curated")',
  'WWW_DIR       <- file.path(ROOT_DIR, "www")',
  '',
  'POLIO_EMAIL_FP <- file.path(DASH_DATA_DIR, "polio_africa_email_input.csv")',
  'POLIO_ALERT_FP <- file.path(DASH_DATA_DIR, "polio_alert_input.csv")',
  'POLIO_ISSUE_FP <- file.path(DASH_DATA_DIR, "polio_last_issue.csv")',
  'RCC_CFG_FP     <- file.path(CFG_DIR,       "rcc_country_fv.csv")',
  'RCC_GEOJSON_FP <- file.path(CURATED_DIR,   "africa_countries_rcc.geojson")',
  'LOGO_FP        <- file.path(WWW_DIR,        "africacdc_logo.png")'
)

# Identifier les lignes du bloc PATHS a remplacer
start_idx <- grep("# -{10,}", app_lines)[1]  # premiere ligne de separation "# ---"
end_idx   <- grep("^LOGO_FP", app_lines)[1]  # derniere ligne du bloc PATHS

if (!is.na(start_idx) && !is.na(end_idx) && start_idx < end_idx) {
  app_lines <- c(
    app_lines[seq_len(start_idx - 1)],
    new_path_block,
    app_lines[(end_idx + 1):length(app_lines)]
  )
  cat("   [OK] Bloc PATHS remplace (lignes", start_idx, "a", end_idx, ")\n")
} else {
  # Fallback : remplacer ligne par ligne
  app_lines <- gsub(
    'ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)',
    'ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)',
    app_lines
  )
  app_lines <- gsub(
    'DASH_DIR.*<-.*file\\.path\\(ROOT_DIR, "dashboard"\\)',
    'DASH_DIR <- ROOT_DIR',
    app_lines
  )
  app_lines <- gsub(
    'APP_DIR <- normalizePath\\(getwd\\(\\)[^)]*\\)',
    '',
    app_lines
  )
  # Supprimer le bloc if (!dir.exists(ROOT_DIR)) {...}
  txt <- paste(app_lines, collapse = "\n")
  txt <- gsub(
    "if \\(!dir\\.exists\\(ROOT_DIR\\)\\) \\{[^}]+\\}",
    "",
    txt
  )
  app_lines <- strsplit(txt, "\n")[[1]]
  cat("   [OK] Chemins corriges par substitution\n")
}

writeLines(app_lines, file.path(DASH, "app.R"), useBytes = FALSE)

# Verification syntaxique
ok <- tryCatch({ parse(file = file.path(DASH, "app.R")); TRUE },
               error = function(e) { cat("   [!!] ERREUR SYNTAXE:", e$message, "\n"); FALSE })
if (ok) cat("   [OK] app.R syntaxiquement valide\n")

# ---- 3. Verifier que les fichiers cles sont bien dans dashboard/ ----
cat("\n3. Verification structure dashboard/ :\n")
key_files <- c(
  "app.R",
  "config/rcc_country_fv.csv",
  "data/curated/africa_countries_rcc.geojson",
  "data/dashboard/polio_africa_email_input.csv",
  "data/dashboard/alert_recipients.csv"
)
all_ok <- TRUE
for (f in key_files) {
  fp <- file.path(DASH, f)
  exists <- file.exists(fp)
  cat(sprintf("   %s %s\n", if (exists) "[OK]" else "[!!]", f))
  if (!exists) all_ok <- FALSE
}

if (!all_ok) {
  stop("Fichiers manquants dans dashboard/ - corrigez avant de deployer.", call. = FALSE)
}

# ---- 4. Deploiement ----
cat("\n4. Deploiement sur shinyapps.io...\n")
cat("   (peut prendre 1-2 minutes)\n\n")

rsconnect::deployApp(
  appDir      = DASH,
  appName     = "dashboard",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("\n=== DEPLOIEMENT TERMINE ===\n")
cat("Verifiez : rsconnect::showLogs('dashboard')\n")
