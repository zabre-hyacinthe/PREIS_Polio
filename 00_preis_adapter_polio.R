# =============================================================================
# PREIS — COUCHE D'ADAPTATION MODULE POLIO
# Fichier : 00_preis_adapter_polio.R
# Auteur  : Dr R. Hyacinthe ZABRE — Africa CDC
# Version : 1.0 — 16 juin 2026
#
# RÔLE DE CE SCRIPT
# -----------------
# Transformer les fichiers de sortie du module Polio (format spécifique)
# vers le FORMAT SOCLE COMMUN PREIS, identique à l'adapter Ebola.
#
# Ce module a une logique différente d'Ebola :
#   - Source       : GPEI "Polio This Week" (hebdomadaire, site web)
#   - Granularité  : pays (55 États membres Africa CDC)
#   - Indicateurs  : count d'événements (cas confirmés + échantillons env.)
#   - Signal       : dérivé du virus_type (cVDPV1/2/3, WPV1) + event_type
#   - Portée       : continentale multi-pays (vs. 1 pays pour Ebola)
#
# Il produit les mêmes 4 fichiers standardisés dans data/final/preis_common/ :
#   - preis_series.csv    (série temporelle par pays et par indicateur)
#   - preis_zones.csv     (détail géographique — ici = pays africains)
#   - preis_signals.csv   (signaux d'alerte : tout événement cVDPV ou WPV)
#   - preis_meta.json     (fraîcheur, statut système, métadonnées)
#
# PRINCIPE : seule cette couche change si le format Polio évolue.
#            Le dashboard multi-modules lit toujours le même format commun.
# =============================================================================


# --------------------------------------------------------------------------- #
# 0. CONFIGURATION ET CHEMINS
# --------------------------------------------------------------------------- #

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(lubridate)
  library(tidyr)
})

# Détection automatique du répertoire racine
BASE_DIR <- Sys.getenv("PREIS_POLIO_ROOT",
                       ifelse(dir.exists("D:/PREIS_Polio_FV"),
                              "D:/PREIS_Polio_FV", getwd()))

# Chemins sources (fichiers produits par le pipeline Polio)
PATH_ALERT_INPUT  <- file.path(BASE_DIR,
                               "dashboard/data/dashboard/polio_alert_input.csv")
PATH_EMAIL_INPUT  <- file.path(BASE_DIR,
                               "dashboard/data/dashboard/polio_africa_email_input.csv")
PATH_LAST_ISSUE   <- file.path(BASE_DIR,
                               "dashboard/data/dashboard/polio_last_issue.csv")
PATH_ISSUE_META   <- file.path(BASE_DIR,
                               "data/processed/polio_issue_metadata.csv")
PATH_SEND_LOG     <- file.path(BASE_DIR,
                               "dashboard/data/dashboard/polio_email_send_log.csv")
PATH_COUNTRY_COORDS <- file.path(BASE_DIR, "config/country_coords.csv")
PATH_RCC          <- file.path(BASE_DIR, "config/rcc_country_fv.csv")

# Dossier de sortie format commun (même convention qu'Ebola)
DIR_COMMON <- file.path(BASE_DIR, "data/final/preis_common")
if (!dir.exists(DIR_COMMON)) dir.create(DIR_COMMON, recursive = TRUE)

# Constantes du module
MODULE_ID   <- "polio"
SOURCE_NAME <- "GPEI / Polio This Week"
SOURCE_URL  <- "https://polioeradication.org/about-polio/polio-this-week/"

cat("=== PREIS Adapter — Module:", MODULE_ID, "===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "\n\n")


# --------------------------------------------------------------------------- #
# 1. FONCTIONS UTILITAIRES
# --------------------------------------------------------------------------- #

read_safe <- function(path, ...) {
  if (!file.exists(path)) {
    warning("Fichier source introuvable, ignoré : ", path)
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE, ...)
}

# Dérive le niveau de signal depuis le type de virus et le type d'événement
# Règle épidémiologique :
#   WPV1 (poliovirus sauvage)        → critical  (éradication à risque)
#   cVDPV + cas humain               → high      (transmission active)
#   cVDPV + environnement seulement  → moderate  (circulation silencieuse)
#   Autre / inconnu                  → info
map_polio_signal_level <- function(virus_type, event_type) {
  dplyr::case_when(
    grepl("WPV", virus_type, ignore.case = TRUE)           ~ "critical",
    grepl("case", event_type, ignore.case = TRUE) &
      grepl("cVDPV", virus_type, ignore.case = TRUE)       ~ "high",
    grepl("Environmental", event_type, ignore.case = TRUE) &
      grepl("cVDPV", virus_type, ignore.case = TRUE)       ~ "moderate",
    TRUE                                                    ~ "info"
  )
}

# Dérive le type de valeur Polio
# count = nombre d'événements (cas ou échantillons), toujours incident
map_polio_value_type <- function(event_type) {
  dplyr::case_when(
    grepl("Case", event_type, ignore.case = TRUE)          ~ "incident",
    grepl("Environmental", event_type, ignore.case = TRUE) ~ "incident",
    TRUE                                                    ~ "incident"
  )
}

# Normalise le code d'indicateur depuis virus_type + event_type
# Ex: cVDPV2 + Case → "cvdpv2_cases"
#     cVDPV2 + Environmental → "cvdpv2_env_samples"
#     WPV1   + Case → "wpv1_cases"
make_indicator_code <- function(virus_type, event_type) {
  vt <- tolower(gsub("[^a-zA-Z0-9]", "", virus_type))
  et <- dplyr::case_when(
    grepl("case", event_type, ignore.case = TRUE) &
      grepl("environ", event_type, ignore.case = TRUE) ~ "cases_and_env",
    grepl("case", event_type, ignore.case = TRUE)      ~ "cases",
    grepl("environ", event_type, ignore.case = TRUE)   ~ "env_samples",
    TRUE                                               ~ "events"
  )
  paste0(vt, "_", et)
}

cat("Fonctions utilitaires chargées.\n")


# --------------------------------------------------------------------------- #
# 2. CHARGEMENT DES SOURCES
# --------------------------------------------------------------------------- #

cat("\n-- Chargement des fichiers sources --\n")

# Source principale : données d'alerte (priorité) ou email input
alert_raw   <- read_safe(PATH_ALERT_INPUT)
email_raw   <- read_safe(PATH_EMAIL_INPUT)
last_issue  <- read_safe(PATH_LAST_ISSUE)
issue_meta  <- read_safe(PATH_ISSUE_META)
country_ref <- read_safe(PATH_COUNTRY_COORDS)
rcc_ref     <- read_safe(PATH_RCC)

# Sélection de la source principale (alert_input prioritaire)
events_raw <- if (!is.null(alert_raw) && nrow(alert_raw) > 0) {
  cat("  Source : polio_alert_input.csv\n")
  alert_raw
} else if (!is.null(email_raw) && nrow(email_raw) > 0) {
  cat("  Source : polio_africa_email_input.csv\n")
  email_raw
} else {
  cat("  AVERTISSEMENT : aucune source d'événements disponible.\n")
  NULL
}

# Métadonnées de la dernière issue
if (!is.null(last_issue) && nrow(last_issue) > 0) {
  last_issue_id   <- last_issue$issue_id[1]
  last_issue_date <- as.Date(last_issue$issue_date[1],
                             tryFormats = c("%Y-%m-%d", "%d %B %Y",
                                            "%B %d, %Y"))
  last_source_url <- coalesce(last_issue$source_url[1], SOURCE_URL)
  cat("  Dernière issue :", last_issue_id,
      "(", format(last_issue_date), ")\n")
} else {
  # Fallback depuis les données elles-mêmes
  last_issue_id   <- if (!is.null(events_raw))
    events_raw$issue_id[1] else NA_character_
  last_issue_date <- if (!is.null(events_raw))
    as.Date(events_raw$issue_date[1]) else NA
  last_source_url <- SOURCE_URL
  cat("  Issue déduite des données :", last_issue_id, "\n")
}

# report_id normalisé (format cohérent avec Ebola : "issue_YYYYMMDD")
last_report_id <- paste0("issue_",
                         format(last_issue_date, "%Y%m%d"))


# --------------------------------------------------------------------------- #
# 3. PRODUCTION DE preis_series.csv
#    Une ligne par (issue, pays, indicateur)
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_series.csv --\n")

if (!is.null(events_raw) && nrow(events_raw) > 0) {
  
  preis_series <- events_raw %>%
    # Normalisation des dates
    mutate(
      report_date = as.Date(coalesce(
        suppressWarnings(as.character(reporting_date)),
        suppressWarnings(as.character(issue_date))
      ), tryFormats = c("%Y-%m-%d", "%d %B %Y", "%B %d, %Y")),
      report_id   = paste0("issue_", format(report_date, "%Y%m%d")),
      # Codes standardisés
      indicator   = make_indicator_code(virus_type, event_type),
      value_type  = map_polio_value_type(event_type),
      signal_level = map_polio_signal_level(virus_type, event_type),
      # Géographie
      geo_level   = coalesce(geo_level, "country"),
      geo_name    = tools::toTitleCase(tolower(
        coalesce(country, country_raw, "Unknown"))),
      geo_code    = toupper(coalesce(iso3, NA_character_)),
      # Colonnes socle commun
      module      = MODULE_ID,
      source      = SOURCE_NAME,
      value       = as.numeric(count),
      provisional = FALSE,   # GPEI publie des données validées
      source_url  = coalesce(source_url, last_source_url),
      extracted_at = Sys.time()
    ) %>%
    # Sélection colonnes socle commun
    select(
      module, source, report_id, report_date,
      geo_level, geo_name, geo_code,
      indicator, value, value_type,
      signal_level, provisional,
      source_url, extracted_at
    ) %>%
    filter(!is.na(value), !is.na(geo_name)) %>%
    # Déduplication sur la clé primaire du socle
    distinct(module, report_id, geo_name, indicator, .keep_all = TRUE) %>%
    arrange(report_date, geo_name, indicator)
  
  out_series <- file.path(DIR_COMMON, "preis_series.csv")
  write_csv(preis_series, out_series)
  cat("  preis_series.csv écrit :", nrow(preis_series), "lignes →",
      out_series, "\n")
  
} else {
  preis_series <- NULL
  cat("  preis_series.csv NON produit (données sources manquantes).\n")
}


# --------------------------------------------------------------------------- #
# 4. PRODUCTION DE preis_zones.csv
#    Pour Polio : une ligne par pays actif (avec au moins 1 événement)
#    + enrichissement RCC et coordonnées géographiques
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_zones.csv --\n")

if (!is.null(events_raw) && nrow(events_raw) > 0) {
  
  # Agrégation par pays : total events, max signal level
  country_summary <- events_raw %>%
    mutate(
      geo_name     = tools::toTitleCase(tolower(
        coalesce(country, country_raw, "Unknown"))),
      geo_code     = toupper(coalesce(iso3, NA_character_)),
      rcc_raw      = coalesce(rcc, NA_character_),
      signal_level = map_polio_signal_level(virus_type, event_type),
      n_cases      = ifelse(grepl("case", event_type, ignore.case = TRUE),
                            as.numeric(count), 0),
      n_env        = ifelse(grepl("environ", event_type, ignore.case = TRUE),
                            as.numeric(count), 0),
      n_events     = as.numeric(count)
    ) %>%
    group_by(geo_name, geo_code, rcc_raw) %>%
    summarise(
      total_events = sum(n_events, na.rm = TRUE),
      total_cases  = sum(n_cases,  na.rm = TRUE),
      total_env    = sum(n_env,    na.rm = TRUE),
      # Niveau de signal le plus élevé parmi tous les événements du pays
      signal_level = dplyr::first(signal_level[order(
        match(signal_level,
              c("critical","high","moderate","info","none")))]),
      virus_types  = paste(unique(virus_type), collapse = "; "),
      .groups = "drop"
    )
  
  # Enrichissement avec coordonnées si disponibles
  if (!is.null(country_ref)) {
    country_summary <- country_summary %>%
      left_join(
        country_ref %>% select(iso3, lat, lon),
        by = c("geo_code" = "iso3")
      )
  } else {
    country_summary <- country_summary %>%
      mutate(lat = NA_real_, lon = NA_real_)
  }
  
  # Construction au format socle commun
  # Pour preis_zones, on utilise total_events comme valeur principale
  preis_zones <- country_summary %>%
    mutate(
      module       = MODULE_ID,
      source       = SOURCE_NAME,
      report_id    = last_report_id,
      report_date  = last_issue_date,
      geo_level    = "country",
      indicator    = "polio_events_total",
      value        = total_events,
      value_type   = "incident",
      provisional  = FALSE,
      source_url   = last_source_url,
      extracted_at = Sys.time(),
      # Colonnes supplémentaires spécifiques Polio (conservées en extra)
      polio_cases  = total_cases,
      polio_env    = total_env,
      polio_virus  = virus_types,
      geo_lat      = lat,
      geo_lon      = lon,
      rcc          = rcc_raw
    ) %>%
    select(
      module, source, report_id, report_date,
      geo_level, geo_name, geo_code,
      indicator, value, value_type,
      signal_level, provisional,
      source_url, extracted_at,
      # Colonnes extra Polio (le dashboard peut les utiliser pour la carte)
      polio_cases, polio_env, polio_virus,
      geo_lat, geo_lon, rcc
    ) %>%
    filter(!is.na(value), value > 0) %>%
    arrange(desc(signal_level), desc(value))
  
  out_zones <- file.path(DIR_COMMON, "preis_zones.csv")
  write_csv(preis_zones, out_zones)
  cat("  preis_zones.csv écrit :", nrow(preis_zones), "pays →",
      out_zones, "\n")
  
} else {
  preis_zones <- NULL
  cat("  preis_zones.csv NON produit (données sources manquantes).\n")
}


# --------------------------------------------------------------------------- #
# 5. PRODUCTION DE preis_signals.csv
#    Pour Polio : chaque événement cVDPV/WPV = un signal
#    (contrairement à Ebola où les signaux sont calculés)
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_signals.csv --\n")

if (!is.null(events_raw) && nrow(events_raw) > 0) {
  
  preis_signals <- events_raw %>%
    mutate(
      report_date  = as.Date(coalesce(
        suppressWarnings(as.character(reporting_date)),
        suppressWarnings(as.character(issue_date))
      ), tryFormats = c("%Y-%m-%d", "%d %B %Y", "%B %d, %Y")),
      report_id    = paste0("issue_", format(report_date, "%Y%m%d")),
      geo_level    = coalesce(geo_level, "country"),
      geo_name     = tools::toTitleCase(tolower(
        coalesce(country, country_raw, "Unknown"))),
      geo_code     = toupper(coalesce(iso3, NA_character_)),
      # Pour Polio : l'indicateur du signal = type de virus détecté
      indicator    = coalesce(virus_type, "polio_unknown"),
      value        = as.numeric(count),
      value_type   = map_polio_value_type(event_type),
      signal_level = map_polio_signal_level(virus_type, event_type),
      module       = MODULE_ID,
      source       = SOURCE_NAME,
      provisional  = FALSE,
      source_url   = coalesce(source_url, last_source_url),
      extracted_at = Sys.time(),
      # Détail du signal (équivalent du "detail" + "hypotheses" Ebola)
      signal_detail = paste0(
        coalesce(event_type, ""), ": ",
        coalesce(summary_text, ""),
        ifelse(!is.na(raw_bullet), paste0(" [", raw_bullet, "]"), "")
      ),
      signal_hypotheses = dplyr::case_when(
        signal_level == "critical"  ~
          "WPV détecté : risque de ré-introduction. Vérifier couverture vaccinale, renforcer surveillance.",
        signal_level == "high"      ~
          "Cas humain cVDPV confirmé : transmission active probable. Campagne de vaccination urgente.",
        signal_level == "moderate"  ~
          "Circulation environnementale cVDPV : surveillance renforcée, investigation zone de couverture vaccin.",
        TRUE                        ~
          "Événement polio à surveiller. Vérifier statut vaccinal de la zone."
      )
    ) %>%
    select(
      module, source, report_id, report_date,
      geo_level, geo_name, geo_code,
      indicator, value, value_type,
      signal_level, provisional,
      source_url, extracted_at,
      signal_detail, signal_hypotheses
    ) %>%
    filter(!is.na(geo_name)) %>%
    # Tri par criticité
    arrange(
      match(signal_level, c("critical","high","moderate","info","none")),
      geo_name
    ) %>%
    distinct()
  
  out_signals <- file.path(DIR_COMMON, "preis_signals.csv")
  write_csv(preis_signals, out_signals)
  cat("  preis_signals.csv écrit :", nrow(preis_signals), "signaux →",
      out_signals, "\n")
  
} else {
  preis_signals <- NULL
  cat("  preis_signals.csv NON produit (données sources manquantes).\n")
}


# --------------------------------------------------------------------------- #
# 6. PRODUCTION DE preis_meta.json
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_meta.json --\n")

n_signals <- if (!is.null(preis_signals)) nrow(preis_signals) else 0
n_zones   <- if (!is.null(preis_zones))   nrow(preis_zones)   else 0
n_series  <- if (!is.null(preis_series))  nrow(preis_series)  else 0

# Résumé des signaux par niveau
signals_summary <- list(critical = 0L, high = 0L,
                        moderate = 0L, info = 0L)
if (!is.null(preis_signals) && nrow(preis_signals) > 0) {
  sig_counts <- preis_signals %>%
    count(signal_level) %>%
    tibble::deframe()
  for (lv in names(signals_summary)) {
    if (lv %in% names(sig_counts))
      signals_summary[[lv]] <- as.integer(sig_counts[lv])
  }
}

# Résumé par type de virus
virus_summary <- list()
if (!is.null(events_raw) && nrow(events_raw) > 0) {
  virus_summary <- events_raw %>%
    group_by(virus_type) %>%
    summarise(
      n_events   = sum(as.numeric(count), na.rm = TRUE),
      n_countries = n_distinct(coalesce(iso3, country_raw)),
      .groups = "drop"
    ) %>%
    split(.$virus_type) %>%
    lapply(function(x) list(
      n_events    = x$n_events,
      n_countries = x$n_countries
    ))
}

# Pays affectés (signal high ou critical)
countries_alert <- character(0)
if (!is.null(preis_signals)) {
  countries_alert <- preis_signals %>%
    filter(signal_level %in% c("critical", "high")) %>%
    pull(geo_name) %>%
    unique() %>%
    sort()
}

# Statut système
system_status <- dplyr::case_when(
  signals_summary$critical > 0 ~ "critical",
  signals_summary$high     > 0 ~ "alert",
  signals_summary$moderate > 0 ~ "warning",
  n_series > 0                 ~ "ok",
  TRUE                         ~ "no_data"
)

meta <- list(
  module            = MODULE_ID,
  module_label      = "Polio — Afrique (cVDPV & WPV)",
  source            = SOURCE_NAME,
  last_report_id    = last_report_id,
  last_report_date  = as.character(last_issue_date),
  last_extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  system_status     = system_status,
  n_series_rows     = n_series,
  n_countries       = n_zones,
  n_signals         = n_signals,
  signals_by_level  = signals_summary,
  virus_summary     = virus_summary,
  countries_alert   = as.list(countries_alert),
  source_url        = last_source_url,
  geographic_scope  = list(
    region      = "Africa",
    geo_level   = "country",
    n_countries = n_zones,
    rcc_covered = c("Northern", "Southern", "Eastern",
                    "Western", "Central")
  ),
  methodology_notes = list(
    signal_rule    = paste0(
      "WPV = critical ; cVDPV + cas humain = high ; ",
      "cVDPV environnement seul = moderate"),
    data_source    = "GPEI Polio This Week — données validées GPEI/OMS",
    provisional    = "FALSE — source officielle GPEI",
    update_freq    = "Hebdomadaire (mardi/mercredi)"
  ),
  schema_version  = "1.0",
  adapter_version = "00_preis_adapter_polio.R v1.0"
)

out_meta <- file.path(DIR_COMMON, "preis_meta.json")
write_json(meta, out_meta, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("  preis_meta.json écrit →", out_meta, "\n")
cat("  Statut système :", system_status, "\n")


# --------------------------------------------------------------------------- #
# 7. VÉRIFICATION FINALE ET RAPPORT
# --------------------------------------------------------------------------- #

cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ — PREIS Adapter Module:", MODULE_ID, "\n")
cat(strrep("=", 60), "\n")

files_produced <- c(
  "preis_series.csv"  = file.path(DIR_COMMON, "preis_series.csv"),
  "preis_zones.csv"   = file.path(DIR_COMMON, "preis_zones.csv"),
  "preis_signals.csv" = file.path(DIR_COMMON, "preis_signals.csv"),
  "preis_meta.json"   = file.path(DIR_COMMON, "preis_meta.json")
)

for (fname in names(files_produced)) {
  fpath  <- files_produced[fname]
  exists <- file.exists(fpath)
  size   <- if (exists) paste0(round(file.size(fpath)/1024, 1), " KB") else "—"
  status <- if (exists) "OK  ✓" else "MANQUANT ✗"
  cat(sprintf("  %-22s %s  [%s]\n", fname, status, size))
}

cat("\nDernière issue traitée    :", last_report_id,
    "(", as.character(last_issue_date), ")\n")
cat("Pays avec événements      :", n_zones, "\n")
cat("Lignes série temporelle   :", n_series, "\n")
cat("Signaux générés           :", n_signals,
    sprintf("(critique:%d | élevé:%d | modéré:%d | info:%d)\n",
            signals_summary$critical, signals_summary$high,
            signals_summary$moderate, signals_summary$info))

if (length(countries_alert) > 0) {
  cat("Pays en alerte (high+)    :",
      paste(countries_alert, collapse = ", "), "\n")
}

cat("Statut système            :", toupper(system_status), "\n")
cat(strrep("=", 60), "\n")
cat("\nAdapter terminé :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Retourner invisiblement les 4 objets pour usage dans un pipeline parent
invisible(list(
  series  = preis_series,
  zones   = preis_zones,
  signals = preis_signals,
  meta    = meta
))

# FIN : 00_preis_adapter_polio.R