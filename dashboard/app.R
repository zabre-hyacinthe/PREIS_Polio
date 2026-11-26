## ============================================================
## PREIS_Polio_FV ? FINAL APP.R
## strict revision: only requested map/filter/icon fixes
## + automatic refresh of source files
## stable blinking markers without divIcon
## ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(sf)
  library(htmltools)
  library(scales)
  library(tibble)
})

options(shiny.maxRequestSize = 100 * 1024^2)
if (requireNamespace("sf", quietly = TRUE)) {
  sf::sf_use_s2(FALSE)
}

# ------------------------------------------------------------
# PATHS - chemins relatifs pour shinyapps.io
# Sur shinyapps.io : getwd() = /srv/connect/apps/dashboard
# config/ et data/ sont copies dans dashboard/ avant deploiement
# ------------------------------------------------------------
ROOT_DIR      <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DASH_DIR      <- ROOT_DIR
DASH_DATA_DIR <- file.path(ROOT_DIR, "data", "dashboard")
CFG_DIR       <- file.path(ROOT_DIR, "config")
CURATED_DIR   <- file.path(ROOT_DIR, "data", "curated")
WWW_DIR       <- file.path(ROOT_DIR, "www")

# Base GitHub raw - donnees dynamiques mises a jour par le pipeline cloud.
# Le dashboard lit d'abord GitHub (donnees fraiches), repli local si offline.
GH_RAW_POLIO <- Sys.getenv(
  "PREIS_POLIO_GH_RAW",
  "https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Polio/refs/heads/main/dashboard/data/dashboard")

# Chemins locaux (repli)
POLIO_EMAIL_LOCAL <- file.path(DASH_DATA_DIR, "polio_africa_email_input.csv")
POLIO_ALERT_LOCAL <- file.path(DASH_DATA_DIR, "polio_alert_input.csv")
POLIO_ISSUE_LOCAL <- file.path(DASH_DATA_DIR, "polio_last_issue.csv")
POLIO_HISTORY_LOCAL <- file.path(DASH_DATA_DIR, "polio_history_dashboard.csv")

# URLs GitHub (prioritaire)
POLIO_EMAIL_URL <- paste0(GH_RAW_POLIO, "/polio_africa_email_input.csv")
POLIO_ALERT_URL <- paste0(GH_RAW_POLIO, "/polio_alert_input.csv")
POLIO_ISSUE_URL <- paste0(GH_RAW_POLIO, "/polio_last_issue.csv")
POLIO_HISTORY_URL <- paste0(GH_RAW_POLIO, "/polio_history_dashboard.csv")

# Noms d'origine = chemins locaux (pour les verifications d'existence ci-dessous)
POLIO_EMAIL_FP <- POLIO_EMAIL_LOCAL
POLIO_ALERT_FP <- POLIO_ALERT_LOCAL
POLIO_ISSUE_FP <- POLIO_ISSUE_LOCAL
RCC_CFG_FP     <- file.path(CFG_DIR,       "rcc_country_fv.csv")
RCC_GEOJSON_FP <- file.path(CURATED_DIR,   "africa_countries_rcc.geojson")
LOGO_FP        <- file.path(WWW_DIR,        "africacdc_logo.png")

if (!dir.exists(DASH_DATA_DIR)) stop("Missing dashboard data dir: ", DASH_DATA_DIR)
if (!file.exists(POLIO_EMAIL_FP) && !file.exists(POLIO_ALERT_FP)) {
  stop("Missing polio dashboard input files in: ", DASH_DATA_DIR)
}
if (!file.exists(RCC_CFG_FP)) stop("Missing RCC mapping file: ", RCC_CFG_FP)
if (!file.exists(RCC_GEOJSON_FP)) stop("Missing RCC geojson file: ", RCC_GEOJSON_FP)

cat("[APP] ROOT_DIR       :", ROOT_DIR, "\n")
cat("[APP] POLIO_EMAIL_FP :", POLIO_EMAIL_FP, "\n")
cat("[APP] POLIO_ALERT_FP :", POLIO_ALERT_FP, "\n")
cat("[APP] POLIO_ISSUE_FP :", POLIO_ISSUE_FP, "\n")
cat("[APP] RCC_CFG_FP     :", RCC_CFG_FP, "\n")
cat("[APP] RCC_GEOJSON_FP :", RCC_GEOJSON_FP, "\n")

# ------------------------------------------------------------
# AUTO REFRESH
# ------------------------------------------------------------
AUTO_REFRESH_MS <- 60 * 1000

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
na2empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

norm_txt <- function(x) {
  x <- na2empty(x)
  x <- str_replace_all(x, "[\r\n\t]", " ")
  x <- str_squish(x)
  x
}

norm_key <- function(x) {
  x <- na2empty(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  x <- str_squish(x)
  x
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_colnames <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_+|_+$", "")
  x
}

safe_read_csv <- function(path) {
  # Si c'est une URL : telecharger via download.file (robuste derriere proxy),
  # puis lire le fichier temporaire. Sinon : lecture locale classique.
  is_url <- grepl("^https?://", path)
  if (is_url) {
    tmp <- tempfile(fileext = ".csv")
    ok <- tryCatch({
      utils::download.file(path, destfile = tmp, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("[DOWNLOAD ERROR] ", path, " :: ", e$message)
      FALSE
    })
    if (!ok || !file.exists(tmp) || file.size(tmp) == 0) return(tibble())
    return(tryCatch(
      readr::read_csv(tmp, show_col_types = FALSE, progress = FALSE),
      error = function(e) {
        message("[READ ERROR] ", path, " :: ", e$message)
        tibble()
      }
    ))
  }
  if (!file.exists(path)) return(tibble())
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("[READ ERROR] ", path, " :: ", e$message)
      tibble()
    }
  )
}

# Lecture avec priorite GitHub puis repli local (pour donnees dynamiques)
read_dynamic <- function(url, local_fp) {
  d <- safe_read_csv(url)
  if (nrow(d) > 0) {
    message("[APP] donnees lues depuis GitHub: ", basename(local_fp))
    return(d)
  }
  message("[APP] repli local pour: ", basename(local_fp))
  safe_read_csv(local_fp)
}

# Etiquettes de chiffres des graphiques (27 oct. 2026) : plotly ignore hjust et
# vjust de geom_text, si bien que les nombres se retrouvaient centres sur le bout
# des barres (illisibles). On decale donc la POSITION de l'etiquette (valeur +
# marge proportionnelle au maximum affiche), ce qui donne le meme rendu avec plotly.
label_nudge <- function(v, frac = 0.07) {
  m <- suppressWarnings(max(v, na.rm = TRUE))
  if (!is.finite(m) || m <= 0) m <- 1
  m * frac
}

empty_plotly <- function(msg = "No data available.") {
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        x = 0.5, y = 0.5,
        text = as.character(msg),
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        font = list(size = 14)
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

canon_country_name <- function(x) {
  z <- norm_key(x)
  dplyr::case_when(
    z %in% c("congo drc", "democratic republic of the congo", "drc", "dr congo", "rdc", "congo kinshasa", "dem rep congo") ~ "Democratic Republic Of The Congo",
    z %in% c("congo republic", "republic of the congo", "republic of congo", "congo brazzaville") ~ "Congo Republic",
    z %in% c("cote d ivoire", "cote divoire", "ivory coast", "cote d'ivoire", "cote d?ivoire") ~ "Cote D'Ivoire",
    z %in% c("cape verde", "cabo verde") ~ "Cabo Verde",
    z %in% c("eswatini", "swaziland") ~ "Eswatini",
    z %in% c("gambia", "the gambia") ~ "Gambia",
    z %in% c("united republic of tanzania", "tanzania") ~ "Tanzania",
    z %in% c("south sudan", "republic of south sudan") ~ "South Sudan",
    z %in% c("guinea bissau", "guinea-bissau") ~ "Guinea-Bissau",
    z %in% c("equatorial guinea", "guinea ecuatorial") ~ "Equatorial Guinea",
    z %in% c("sao tome and principe", "sao tome principe") ~ "Sao Tome And Principe",
    z %in% c("cameroun") ~ "Cameroon",
    z %in% c("tchad") ~ "Chad",
    z == "" ~ "",
    TRUE ~ str_to_title(z)
  )
}

clean_rcc <- function(x) {
  z <- norm_key(x)
  dplyr::case_when(
    z %in% c("north", "north africa", "northern africa", "northern") ~ "Northern",
    z %in% c("west", "west africa", "western africa", "western") ~ "Western",
    z %in% c("central", "central africa", "centre") ~ "Central",
    z %in% c("east", "east africa", "eastern africa", "eastern") ~ "Eastern",
    z %in% c("south", "southern africa", "southern") ~ "Southern",
    z == "" ~ "Unspecified",
    TRUE ~ "Unspecified"
  )
}

clean_virus_type <- function(x) {
  z <- str_to_upper(norm_txt(x))
  dplyr::case_when(
    str_detect(z, "CVDPV1") ~ "cVDPV1",
    str_detect(z, "CVDPV2") ~ "cVDPV2",
    str_detect(z, "CVDPV3") ~ "cVDPV3",
    str_detect(z, "CVDPV") ~ "cVDPV (unspecified type)",
    str_detect(z, "WPV1") ~ "WPV1",
    str_detect(z, "WPV") ~ "WPV",
    z == "" ~ "Unspecified",
    TRUE ~ z
  )
}

# Lignes en direct (27 oct. 2026) : le pipeline fournit n_cases (cas humains) et
# n_env_samples (echantillons environnementaux) pour un meme bulletin. Une ligne
# "Case + Environmental" est separee en deux lignes (AFP / Environmental), comme
# dans l'historique (une ligne par virus et par source), pour que les totaux
# humains et environnementaux ne soient ni melanges ni comptes deux fois.
# Une ligne a verifier (count_status = "review") garde un comptage NA.
# Sans ces colonnes (anciens CSV), les donnees sont retournees inchangees.
split_combined_live_rows <- function(df) {
  need <- c("n_cases", "n_env_samples", "count_status")
  if (!all(need %in% names(df))) return(df)
  nc <- suppressWarnings(as.numeric(df$n_cases))
  ne <- suppressWarnings(as.numeric(df$n_env_samples))
  is_comb <- !is.na(df$count_status) & df$count_status == "parsed" &
    !is.na(nc) & !is.na(ne) & nc > 0 & ne > 0
  if (!any(is_comb)) return(df)
  h <- df[is_comb, , drop = FALSE]
  h$count <- nc[is_comb]
  h$signal_type <- "Case"
  h$event_type <- "Case"
  h$detection_source <- "AFP"
  e <- df[is_comb, , drop = FALSE]
  e$count <- ne[is_comb]
  e$signal_type <- "Environmental"
  e$event_type <- "Environmental"
  e$detection_source <- "Environmental"
  dplyr::bind_rows(df[!is_comb, , drop = FALSE], h, e)
}

clean_detection_source <- function(x) {
  z <- str_to_upper(norm_txt(x))
  env_flag <- str_detect(z, "ENV|ENVIRONMENT")
  hum_flag <- str_detect(z, "AFP|CASE|HUMAN|PATIENT|CLINIC|STOOL")
  
  dplyr::case_when(
    env_flag & hum_flag ~ "Both",
    env_flag ~ "Environmental",
    hum_flag ~ "Human",
    z == "" ~ "Unspecified",
    TRUE ~ "Unspecified"
  )
}

parse_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  
  suppressWarnings({
    d1 <- as.Date(strptime(x, "%Y-%m-%d"))
    d2 <- as.Date(strptime(x, "%d/%m/%Y"))
    d3 <- as.Date(strptime(x, "%m/%d/%Y"))
    d4 <- as.Date(strptime(x, "%Y-%m-%d %H:%M:%S"))
    d5 <- as.Date(strptime(x, "%Y-%m-%d %H:%M"))
    d6 <- as.Date(strptime(x, "%Y-%m-%dT%H:%M:%S"))
    d7 <- as.Date(strptime(x, "%Y-%m-%dT%H:%M:%SZ"))
    d8 <- as.Date(strptime(x, "%d-%m-%Y"))
    d9 <- as.Date(strptime(x, "%d %b %Y"))
    d10 <- as.Date(strptime(x, "%d %B %Y"))
  })
  
  dplyr::coalesce(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
}

coalesce_nonempty <- function(...) {
  xs <- list(...)
  if (length(xs) == 0) return(character(0))
  n <- max(vapply(xs, length, integer(1)), 0L)
  if (n == 0) return(character(0))
  
  out <- rep("", n)
  
  for (i in seq_along(xs)) {
    x <- xs[[i]]
    if (length(x) == 0) next
    x <- as.character(x)
    if (length(x) == 1 && n > 1) x <- rep(x, n)
    if (length(x) != n) x <- rep_len(x, n)
    x[is.na(x)] <- ""
    idx <- (out == "" | out == "Unspecified") & x != "" & x != "Unspecified"
    out[idx] <- x[idx]
  }
  
  out
}

polio_detection_icon <- function(virus_type, detection_source) {
  ds <- norm_txt(detection_source)
  
  if (ds == "Both") return("HE")
  if (ds == "Environmental") return("E")
  if (ds == "Human") return("H")
  return("P")
}

make_clickable_link <- function(url, label = "Open source") {
  url <- norm_txt(url)
  ifelse(
    url == "",
    "",
    paste0('<a href="', htmlEscape(url), '" target="_blank">', htmlEscape(label), "</a>")
  )
}

# ------------------------------------------------------------
# PALETTES
# ------------------------------------------------------------
rcc_palette <- c(
  "Northern" = "#E3A008",
  "Western" = "#0F9D58",
  "Central" = "#8E44AD",
  "Eastern" = "#0072B2",
  "Southern" = "#B3261E",
  "Unspecified" = "#9AA0A6"
)
# Mapping pays->RCC hardcode pour corriger les noms GeoJSON non reconnus
.COUNTRY_RCC_OVERRIDE <- c(
  "Sahrawi Arab Democratic Republic"="Northern",
  "Western Sahara"="Northern",
  "Somalia"="Eastern","Somaliland"="Eastern",
  "Kingdom Of Eswatini"="Southern","Eswatini"="Southern",
  "Republic Of Cabo Verde"="Western","Cabo Verde"="Western",
  "Republic Of The Congo"="Central","Congo Republic"="Central",
  "Sao Tome And Principe"="Central",
  "The Gambia"="Western","Gambia"="Western",
  "Cote D'Ivoire"="Western","Ivory Coast"="Western",
  "Democratic Republic Of The Congo"="Central"
)


detect_palette <- c(
  "Human" = "#1F78B4",
  "Environmental" = "#33A02C",
  "Both" = "#6A3D9A",
  "Unspecified" = "#9AA0A6"
)

virus_palette <- c(
  "WPV1" = "#D7191C", "WPV" = "#B2182B",
  "cVDPV1" = "#F28E2B", "cVDPV2" = "#0072BC",
  "cVDPV3" = "#7B2CBF", "cVDPV (unspecified type)" = "#6C757D",
  "Unspecified" = "#BDBDBD"
)

virus_colour <- function(x) {
  out <- unname(virus_palette[as.character(x)])
  out[is.na(out)] <- virus_palette[["Unspecified"]]
  out
}

# ------------------------------------------------------------
# RCC REFERENCE
# ------------------------------------------------------------
rcc_map <- safe_read_csv(RCC_CFG_FP)
names(rcc_map) <- clean_colnames(names(rcc_map))

rcc_map <- rcc_map %>%
  transmute(
    country = canon_country_name(country),
    iso3 = toupper(norm_txt(iso3)),
    rcc = clean_rcc(rcc)
  ) %>%
  distinct()

rcc_map_iso <- rcc_map %>%
  filter(iso3 != "") %>%
  distinct(iso3, .keep_all = TRUE)

fill_rcc_from_map <- function(df, country_col = "country", rcc_col = "rcc", iso3_col = "iso3") {
  if (!country_col %in% names(df)) df[[country_col]] <- ""
  if (!rcc_col %in% names(df)) df[[rcc_col]] <- ""
  if (!iso3_col %in% names(df)) df[[iso3_col]] <- ""
  
  out <- df %>%
    mutate(
      !!country_col := canon_country_name(.data[[country_col]]),
      !!rcc_col := clean_rcc(.data[[rcc_col]]),
      !!iso3_col := toupper(norm_txt(.data[[iso3_col]]))
    ) %>%
    left_join(
      rcc_map %>% select(country, rcc_country_map = rcc, iso3_country_map = iso3),
      by = setNames("country", country_col)
    ) %>%
    left_join(
      rcc_map_iso %>% select(iso3, rcc_iso_map = rcc, country_iso_map = country),
      by = setNames("iso3", iso3_col)
    ) %>%
    mutate(
      !!country_col := coalesce_nonempty(.data[[country_col]], country_iso_map),
      !!rcc_col := coalesce_nonempty(.data[[rcc_col]], rcc_country_map, rcc_iso_map),
      !!iso3_col := coalesce_nonempty(.data[[iso3_col]], iso3_country_map),
      !!rcc_col := clean_rcc(.data[[rcc_col]])
    ) %>%
    select(-rcc_country_map, -iso3_country_map, -rcc_iso_map, -country_iso_map)
  
  out
}

# ------------------------------------------------------------
# LOAD GEOJSON
# ------------------------------------------------------------
africa_map <- sf::read_sf(RCC_GEOJSON_FP, quiet = TRUE) %>%
  st_make_valid()

names(africa_map) <- clean_colnames(names(africa_map))
if (!"country" %in% names(africa_map)) stop("GeoJSON must contain a country column.")

geo_has_iso3 <- "iso3" %in% names(africa_map)
geo_has_country_key <- "country_key" %in% names(africa_map)
geo_has_rcc <- "rcc" %in% names(africa_map)

geo_iso3 <- rep("", nrow(africa_map))
if (geo_has_iso3) {
  geo_iso3 <- toupper(norm_txt(africa_map$iso3))
} else if (geo_has_country_key) {
  geo_iso3 <- toupper(norm_txt(africa_map$country_key))
}

geo_rcc <- rep("Unspecified", nrow(africa_map))
if (geo_has_rcc) {
  geo_rcc <- clean_rcc(africa_map$rcc)
}

africa_map <- africa_map %>%
  mutate(
    country = canon_country_name(country),
    iso3 = geo_iso3,
    rcc = geo_rcc
  ) %>%
  left_join(
    rcc_map %>% rename(rcc_cfg = rcc),
    by = c("country", "iso3")
  ) %>%
  mutate(
    rcc = coalesce_nonempty(rcc, rcc_cfg),
    rcc = clean_rcc(rcc)
  ) %>%
  select(-rcc_cfg)

# ------------------------------------------------------------
# CENTROIDS
# ------------------------------------------------------------
centroids <- tryCatch({
  cent <- suppressWarnings(
    st_transform(africa_map, 3857) %>%
      st_point_on_surface() %>%
      st_transform(4326)
  )
  
  coords <- st_coordinates(cent)
  
  out <- cent %>%
    st_drop_geometry() %>%
    mutate(
      lon = as.numeric(coords[, 1]),
      lat = as.numeric(coords[, 2])
    )
  
  keep_cols <- intersect(c("country", "iso3", "lon", "lat"), names(out))
  out <- out[, keep_cols, drop = FALSE]
  
  if (!"country" %in% names(out)) out$country <- NA_character_
  if (!"iso3" %in% names(out)) out$iso3 <- NA_character_
  
  out %>%
    mutate(
      country = canon_country_name(country),
      iso3 = toupper(norm_txt(iso3))
    ) %>%
    distinct(country, iso3, .keep_all = TRUE)
}, error = function(e) {
  message("[APP] centroid build failed: ", e$message)
  NULL
})

# ------------------------------------------------------------
# POLIO DATA BUILDER
# ------------------------------------------------------------
build_polio_df <- function() {
  polio_history <- read_dynamic(POLIO_HISTORY_URL, POLIO_HISTORY_LOCAL)
  polio_email <- read_dynamic(POLIO_EMAIL_URL, POLIO_EMAIL_LOCAL)
  polio_alert <- read_dynamic(POLIO_ALERT_URL, POLIO_ALERT_LOCAL)
  polio_issue <- read_dynamic(POLIO_ISSUE_URL, POLIO_ISSUE_LOCAL)
  
  names(polio_history) <- clean_colnames(names(polio_history))
  names(polio_email) <- clean_colnames(names(polio_email))
  names(polio_alert) <- clean_colnames(names(polio_alert))
  names(polio_issue) <- clean_colnames(names(polio_issue))
  
  # PREIS_PATCH_WPV1_RCC_TYPES_V1   
  polio_history <- polio_history %>% mutate(rcc = as.character(rcc))   
  polio_email   <- polio_email   %>% mutate(rcc = as.character(rcc))   
  polio_alert   <- polio_alert   %>% mutate(rcc = as.character(rcc))   
  polio_raw <- bind_rows(polio_history, polio_email, polio_alert) %>%
    distinct()
  polio_raw <- split_combined_live_rows(polio_raw)
  if (nrow(polio_raw) == 0) {
    polio_raw <- tibble(
      issue_id = character(),
      issue_date = character(),
      report_date = character(),
      country = character(),
      iso3 = character(),
      rcc = character(),
      virus_type = character(),
      signal_type = character(),
      count = numeric(),
      summary_text = character(),
      source_url = character(),
      fetched_date = character(),
      location_text = character(),
      onset_date = character(),
      pathogen = character(),
      geo_level = character(),
      raw_bullet = character()
    )
  }
  
  add_missing_col <- function(df, nm, default = "") {
    if (!nm %in% names(df)) df[[nm]] <- default
    df
  }
  
  needed_cols <- c(
    "issue_id", "issue_date", "report_date", "country", "iso3", "rcc",
    "virus_type", "signal_type", "detection_source", "count", "summary_text", "source_url",
    "fetched_date", "location_text", "onset_date", "pathogen", "geo_level", "raw_bullet"
  )
  
  for (nm in needed_cols) {
    polio_raw <- add_missing_col(polio_raw, nm)
  }
  
  polio_df <- polio_raw %>%
    transmute(
      issue_id = norm_txt(issue_id),
      issue_date = parse_date_safe(issue_date),
      report_date = parse_date_safe(report_date),
      country = canon_country_name(country),
      iso3 = toupper(norm_txt(iso3)),
      rcc = clean_rcc(rcc),
      virus_type = clean_virus_type(virus_type),
      signal_type = norm_txt(signal_type),
      detection_source = coalesce_nonempty(
        clean_detection_source(detection_source),
        clean_detection_source(signal_type)
      ),
      cases = safe_num(count),
      summary_text = norm_txt(summary_text),
      source_url = norm_txt(source_url),
      fetched_date = norm_txt(fetched_date),
      location_text = norm_txt(location_text),
      onset_date = norm_txt(onset_date),
      pathogen = norm_txt(pathogen),
      geo_level = norm_txt(geo_level),
      raw_bullet = norm_txt(raw_bullet)
    ) %>%
    mutate(
      # 27 oct. 2026 : un comptage manquant reste NA (il n'est plus transforme en 1).
      cases = ifelse(!is.na(cases) & cases < 0, NA_real_, cases),
      issue_date = dplyr::coalesce(issue_date, report_date),
      url_link = make_clickable_link(source_url)
    )
  
  polio_df <- fill_rcc_from_map(polio_df, "country", "rcc", "iso3")
  if (!"lon" %in% names(polio_df)) polio_df$lon <- NA_real_
  if (!"lat" %in% names(polio_df)) polio_df$lat <- NA_real_
  
  if (all(is.na(polio_df$issue_date)) && nrow(polio_issue) > 0) {
    if ("issue_date" %in% names(polio_issue)) {
      polio_df$issue_date <- parse_date_safe(polio_issue$issue_date)[1]
    } else if ("date" %in% names(polio_issue)) {
      polio_df$issue_date <- parse_date_safe(polio_issue$date)[1]
    }
  }
  
  if (!is.null(centroids) && nrow(centroids) > 0) {
    polio_df <- polio_df %>%
      left_join(
        centroids %>%
          rename(
            lon_join_exact = lon,
            lat_join_exact = lat
          ),
        by = c("country", "iso3")
      )
    
    centroids_country <- centroids %>%
      filter(!is.na(country), country != "") %>%
      group_by(country) %>%
      summarise(
        lon_join_country = dplyr::first(lon),
        lat_join_country = dplyr::first(lat),
        .groups = "drop"
      )
    
    polio_df <- polio_df %>%
      left_join(centroids_country, by = "country") %>%
      mutate(
        lon = dplyr::coalesce(lon, lon_join_exact, lon_join_country),
        lat = dplyr::coalesce(lat, lat_join_exact, lat_join_country)
      ) %>%
      select(-lon_join_exact, -lat_join_exact, -lon_join_country, -lat_join_country)
  }
  
  polio_df <- polio_df %>%
    mutate(
      detection_source = ifelse(is.na(detection_source) | detection_source == "", "Unspecified", detection_source),
      icon = mapply(polio_detection_icon, virus_type, detection_source, USE.NAMES = FALSE)
    ) %>%
    arrange(desc(issue_date)) %>%
    distinct(issue_date, country, iso3, virus_type, detection_source, cases,
             summary_text, .keep_all = TRUE) %>%
    arrange(issue_date, country, virus_type, detection_source)
  
  polio_df
}

# initial load only for UI defaults
polio_df_init <- build_polio_df()

cat("[APP] polio_df rows:", nrow(polio_df_init), "\n")
cat("[APP] polio_df with coordinates:", sum(!is.na(polio_df_init$lon) & !is.na(polio_df_init$lat)), "\n")
cat("[APP] unique countries in polio_df:", length(unique(polio_df_init$country)), "\n")

# ------------------------------------------------------------
# SPREAD MARKERS
# ------------------------------------------------------------
spread_country_markers <- function(df) {
  if (nrow(df) == 0) return(df)
  
  df %>%
    group_by(country) %>%
    mutate(
      n_group = n(),
      rank_group = row_number(),
      angle = 2 * pi * (rank_group - 1) / pmax(n_group, 1),
      radius = dplyr::case_when(
        n_group <= 1 ~ 0,
        n_group <= 4 ~ 0.45,
        n_group <= 8 ~ 0.70,
        TRUE ~ 0.90
      ),
      lon = lon + radius * cos(angle),
      lat = lat + (radius * sin(angle) * 0.60)
    ) %>%
    ungroup() %>%
    select(-n_group, -rank_group, -angle, -radius)
}

# ------------------------------------------------------------
# FILTER VALUES
# ------------------------------------------------------------
all_dates <- polio_df_init$issue_date[!is.na(polio_df_init$issue_date)]
if (length(all_dates) == 0) {
  date_min <- Sys.Date() - 30
  date_max <- Sys.Date()
} else {
  date_min <- min(all_dates)
  date_max <- max(all_dates)
}

rcc_levels <- sort(unique(polio_df_init$rcc))
rcc_levels <- rcc_levels[rcc_levels != "" & !is.na(rcc_levels)]

country_levels <- sort(unique(polio_df_init$country))
country_levels <- country_levels[country_levels != "" & !is.na(country_levels)]

virus_levels <- sort(unique(polio_df_init$virus_type))
virus_levels <- virus_levels[virus_levels != "" & !is.na(virus_levels)]

detect_levels <- c("Human", "Environmental", "Both", "Unspecified")
detect_levels <- detect_levels[detect_levels %in% unique(polio_df_init$detection_source)]

year_levels <- sort(unique(as.integer(format(polio_df_init$issue_date, "%Y"))))
year_levels <- year_levels[!is.na(year_levels)]
if (!length(year_levels)) year_levels <- as.integer(format(Sys.Date(), "%Y"))
latest_year <- max(year_levels)

# Neutralite institutionnelle (27 oct. 2026) : logo Africa CDC retire de l'en-tete.
# Version precedente sauvegardee dans ARCHIVE_UNUSED/backups_20261027_etape2/
header_title <- tags$span(
  tags$span("PREIS-POLIO", class = "preis-header-title")
)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
# ---- Interface language (EN/FR/PT/ES/AR/SW): client-side translation engine copied from the PREIS AI Tracker (26 Nov 2026).
# Translations are PROVISIONAL (AI-drafted), to be reviewed by native speakers. Filter VALUES (virus types, countries) are not translated.
PREIS_I18N_JS <- r"---((function(){
  var L = {"fr": {"Trends & quality": "Tendances et qualit\u00e9", "Map": "Carte", "RCC": "CCR", "Member State": "\u00c9tat membre", "Virus type": "Type de virus", "Detection source": "Source de d\u00e9tection", "Publication year": "Ann\u00e9e de publication", "GPEI publication date": "Date de publication GPEI", "Latest": "Derni\u00e8re", "Full history": "Historique complet", "Show RCC layer": "Afficher la couche CCR", "Show polio markers": "Afficher les marqueurs polio", "All": "Tous", "African Poliovirus Epidemic Intelligence Dashboard": "Tableau de bord de renseignement \u00e9pid\u00e9mique sur le poliovirus en Afrique", "Automated monitoring of publicly reported poliovirus signals in Africa": "Suivi automatis\u00e9 des signaux de poliovirus publi\u00e9s en Afrique", "Total detections (human + environmental)": "Total des d\u00e9tections (humaines + environnementales)", "Affected Member States": "\u00c9tats membres touch\u00e9s", "RCC represented": "CCR repr\u00e9sent\u00e9s", "No dated issue": "Aucune \u00e9dition dat\u00e9e", "Latest issue date": "Date de la derni\u00e8re \u00e9dition", "Human detections": "D\u00e9tections humaines", "Environmental detections": "D\u00e9tections environnementales", "Polio Africa map": "Carte de la polio en Afrique", "RCC and epidemiological profile": "CCR et profil \u00e9pid\u00e9miologique", "Temporal evolution of detections": "\u00c9volution temporelle des d\u00e9tections", "Operational interpretation": "Interpr\u00e9tation op\u00e9rationnelle", "This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI.": "Ce tableau de bord est un prototype non officiel. Il affiche les signaux de polio publi\u00e9s par le GPEI (Polio This Week). Le regroupement par CCR sert uniquement de classification g\u00e9ographique ; ce n'est pas un produit officiel de l'Africa CDC, de l'OMS ou du GPEI.", "Alerts are prepared for RCCs; sending is currently limited to the author.": "Les alertes sont pr\u00e9par\u00e9es pour les CCR ; l'envoi est actuellement limit\u00e9 \u00e0 l'auteur.", "Affected Member States are shown with polio markers.": "Les \u00c9tats membres touch\u00e9s sont indiqu\u00e9s par des marqueurs polio.", "The dashboard distinguishes detections in humans and environmental surveillance.": "Le tableau de bord distingue les d\u00e9tections chez l'humain et celles de la surveillance environnementale.", "The same filters drive the map, charts, RCC summaries, country details, and downloadable data.": "Les m\u00eames filtres pilotent la carte, les graphiques, les r\u00e9sum\u00e9s par CCR, le d\u00e9tail par pays et les donn\u00e9es t\u00e9l\u00e9chargeables.", "RCC summary": "R\u00e9sum\u00e9 par CCR", "Country details": "D\u00e9tail par pays", "Interpretation rule": "R\u00e8gle d'interpr\u00e9tation", "These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date.": "Il s'agit de d\u00e9tections rapport\u00e9es par date de publication du GPEI, et non de l'incidence selon la date de d\u00e9but de la paralysie ou de pr\u00e9l\u00e8vement.", "Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.": "Les publications manquantes sont trait\u00e9es comme des donn\u00e9es indisponibles, jamais comme z\u00e9ro. La couverture historique varie selon l'ann\u00e9e.", "Detections reported by GPEI publication date": "D\u00e9tections rapport\u00e9es par date de publication du GPEI", "Archive coverage by year": "Couverture de l'archive par ann\u00e9e", "Reported detections by virus type": "D\u00e9tections rapport\u00e9es par type de virus", "Member States with most reported detections": "\u00c9tats membres avec le plus de d\u00e9tections rapport\u00e9es", "Historical data quality and coverage": "Qualit\u00e9 et couverture des donn\u00e9es historiques", "Filtered polio records": "Enregistrements polio filtr\u00e9s", "Download filtered data": "T\u00e9l\u00e9charger les donn\u00e9es filtr\u00e9es", "Polio records (filtered)": "Enregistrements polio (filtr\u00e9s)", "Note": "Note", "This app uses the real PREIS_Polio_FV input files.": "Cette application utilise les vrais fichiers d'entr\u00e9e de PREIS_Polio_FV.", "It is aligned with the project RCC mapping and Africa RCC geojson.": "Elle est align\u00e9e sur la cartographie CCR du projet et le geojson des CCR d'Afrique.", "The exported file includes virus type and detection source.": "Le fichier export\u00e9 inclut le type de virus et la source de d\u00e9tection.", "count to verify": "comptage \u00e0 v\u00e9rifier", "None in filtered data": "Aucun dans les donn\u00e9es filtr\u00e9es", "RCC layer": "Couche CCR", "Polio markers": "Marqueurs polio", "Africa CDC RCC": "CCR de l'Africa CDC", "Records:": "Enregistrements :", "Virus type(s):": "Type(s) de virus :", "Detection source(s):": "Source(s) de d\u00e9tection :", "Detections": "D\u00e9tections", "Reported detections": "D\u00e9tections rapport\u00e9es", "Available publication dates": "Dates de publication disponibles", "Available GPEI publications; missing dates are not zero": "Publications GPEI disponibles ; les dates manquantes ne sont pas des z\u00e9ros", "Human": "Humain", "Environmental": "Environnemental", "Both": "Les deux", "Unspecified": "Non pr\u00e9cis\u00e9", "No RCC data available.": "Aucune donn\u00e9e CCR disponible.", "No virus type data available.": "Aucune donn\u00e9e sur le type de virus disponible.", "No detection source data available.": "Aucune donn\u00e9e sur la source de d\u00e9tection disponible.", "No dated time series available.": "Aucune s\u00e9rie temporelle dat\u00e9e disponible.", "No historical publication data for these filters.": "Aucune donn\u00e9e de publication historique pour ces filtres.", "No coverage data for these filters.": "Aucune donn\u00e9e de couverture pour ces filtres.", "No virus data for these filters.": "Aucune donn\u00e9e de virus pour ces filtres.", "No Member State data for these filters.": "Aucune donn\u00e9e d'\u00c9tat membre pour ces filtres.", "Northern": "Nord", "Western": "Ouest", "Central": "Centre", "Eastern": "Est", "Southern": "Sud", "Search:": "Rechercher :", "Show": "Afficher", "entries": "entr\u00e9es", "Previous": "Pr\u00e9c\u00e9dent", "Next": "Suivant", "First": "Premier", "Last": "Dernier", "No data available in table": "Aucune donn\u00e9e disponible dans le tableau", "No matching records found": "Aucun enregistrement correspondant", "countries": "pays", "total_detections": "d\u00e9tections_totales", "human_cases": "cas_humains", "environmental_detections": "d\u00e9tections_environnementales", "records_to_verify": "enregistrements_\u00e0_v\u00e9rifier", "latest_issue": "derni\u00e8re_\u00e9dition", "issue_date": "date_\u00e9dition", "issue_id": "id_\u00e9dition", "country": "pays", "rcc": "ccr", "virus_type": "type_de_virus", "detection_source": "source_de_d\u00e9tection", "signal_type": "type_de_signal", "cases": "cas", "summary_text": "texte_r\u00e9sum\u00e9", "link": "lien", "year": "ann\u00e9e", "Year:": "Ann\u00e9e :", "Date:": "Date :", "Publication date:": "Date de publication :", "Virus type:": "Type de virus :", "Detection source:": "Source de d\u00e9tection :", "Virus:": "Virus :", "Member State:": "\u00c9tat membre :", "Overview": "Vue d'ensemble", "Downloads": "T\u00e9l\u00e9chargements", "Records": "Enregistrements"}, "pt": {"Trends & quality": "Tend\u00eancias e qualidade", "Map": "Mapa", "RCC": "CCR", "Member State": "Estado-Membro", "Virus type": "Tipo de v\u00edrus", "Detection source": "Fonte de dete\u00e7\u00e3o", "Publication year": "Ano de publica\u00e7\u00e3o", "GPEI publication date": "Data de publica\u00e7\u00e3o GPEI", "Latest": "Mais recente", "Full history": "Hist\u00f3rico completo", "Show RCC layer": "Mostrar camada CCR", "Show polio markers": "Mostrar marcadores de poliomielite", "All": "Todos", "African Poliovirus Epidemic Intelligence Dashboard": "Painel de intelig\u00eancia epid\u00e9mica sobre poliov\u00edrus em \u00c1frica", "Automated monitoring of publicly reported poliovirus signals in Africa": "Monitoriza\u00e7\u00e3o automatizada de sinais de poliov\u00edrus divulgados publicamente em \u00c1frica", "Total detections (human + environmental)": "Total de dete\u00e7\u00f5es (humanas + ambientais)", "Affected Member States": "Estados-Membros afetados", "RCC represented": "CCR representados", "No dated issue": "Nenhuma edi\u00e7\u00e3o datada", "Latest issue date": "Data da \u00faltima edi\u00e7\u00e3o", "Human detections": "Dete\u00e7\u00f5es humanas", "Environmental detections": "Dete\u00e7\u00f5es ambientais", "Polio Africa map": "Mapa da poliomielite em \u00c1frica", "RCC and epidemiological profile": "CCR e perfil epidemiol\u00f3gico", "Temporal evolution of detections": "Evolu\u00e7\u00e3o temporal das dete\u00e7\u00f5es", "Operational interpretation": "Interpreta\u00e7\u00e3o operacional", "This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI.": "Este painel \u00e9 um prot\u00f3tipo n\u00e3o oficial. Apresenta os sinais de poliomielite publicados pela GPEI (Polio This Week). O agrupamento por CCR \u00e9 usado apenas como classifica\u00e7\u00e3o geogr\u00e1fica; n\u00e3o \u00e9 um produto oficial do Africa CDC, da OMS ou da GPEI.", "Alerts are prepared for RCCs; sending is currently limited to the author.": "Os alertas s\u00e3o preparados para os CCR; o envio est\u00e1 atualmente limitado ao autor.", "Affected Member States are shown with polio markers.": "Os Estados-Membros afetados s\u00e3o indicados com marcadores de poliomielite.", "The dashboard distinguishes detections in humans and environmental surveillance.": "O painel distingue as dete\u00e7\u00f5es em humanos e na vigil\u00e2ncia ambiental.", "The same filters drive the map, charts, RCC summaries, country details, and downloadable data.": "Os mesmos filtros controlam o mapa, os gr\u00e1ficos, os resumos por CCR, os detalhes por pa\u00eds e os dados descarreg\u00e1veis.", "RCC summary": "Resumo por CCR", "Country details": "Detalhes por pa\u00eds", "Interpretation rule": "Regra de interpreta\u00e7\u00e3o", "These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date.": "S\u00e3o dete\u00e7\u00f5es comunicadas por data de publica\u00e7\u00e3o da GPEI, e n\u00e3o a incid\u00eancia por data de in\u00edcio da paralisia ou de colheita da amostra.", "Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.": "As publica\u00e7\u00f5es em falta s\u00e3o tratadas como dados indispon\u00edveis, nunca como zero. A cobertura hist\u00f3rica varia consoante o ano.", "Detections reported by GPEI publication date": "Dete\u00e7\u00f5es comunicadas por data de publica\u00e7\u00e3o da GPEI", "Archive coverage by year": "Cobertura do arquivo por ano", "Reported detections by virus type": "Dete\u00e7\u00f5es comunicadas por tipo de v\u00edrus", "Member States with most reported detections": "Estados-Membros com mais dete\u00e7\u00f5es comunicadas", "Historical data quality and coverage": "Qualidade e cobertura dos dados hist\u00f3ricos", "Filtered polio records": "Registos de poliomielite filtrados", "Download filtered data": "Descarregar os dados filtrados", "Polio records (filtered)": "Registos de poliomielite (filtrados)", "Note": "Nota", "This app uses the real PREIS_Polio_FV input files.": "Esta aplica\u00e7\u00e3o utiliza os ficheiros de entrada reais do PREIS_Polio_FV.", "It is aligned with the project RCC mapping and Africa RCC geojson.": "Est\u00e1 alinhada com o mapeamento de CCR do projeto e o geojson dos CCR de \u00c1frica.", "The exported file includes virus type and detection source.": "O ficheiro exportado inclui o tipo de v\u00edrus e a fonte de dete\u00e7\u00e3o.", "count to verify": "contagem a verificar", "None in filtered data": "Nenhum nos dados filtrados", "RCC layer": "Camada CCR", "Polio markers": "Marcadores de poliomielite", "Africa CDC RCC": "CCR do Africa CDC", "Records:": "Registos:", "Virus type(s):": "Tipo(s) de v\u00edrus:", "Detection source(s):": "Fonte(s) de dete\u00e7\u00e3o:", "Detections": "Dete\u00e7\u00f5es", "Reported detections": "Dete\u00e7\u00f5es comunicadas", "Available publication dates": "Datas de publica\u00e7\u00e3o dispon\u00edveis", "Available GPEI publications; missing dates are not zero": "Publica\u00e7\u00f5es GPEI dispon\u00edveis; as datas em falta n\u00e3o s\u00e3o zero", "Human": "Humano", "Environmental": "Ambiental", "Both": "Ambos", "Unspecified": "N\u00e3o especificado", "No RCC data available.": "Sem dados de CCR dispon\u00edveis.", "No virus type data available.": "Sem dados de tipo de v\u00edrus dispon\u00edveis.", "No detection source data available.": "Sem dados de fonte de dete\u00e7\u00e3o dispon\u00edveis.", "No dated time series available.": "Sem s\u00e9rie temporal datada dispon\u00edvel.", "No historical publication data for these filters.": "Sem dados hist\u00f3ricos de publica\u00e7\u00e3o para estes filtros.", "No coverage data for these filters.": "Sem dados de cobertura para estes filtros.", "No virus data for these filters.": "Sem dados de v\u00edrus para estes filtros.", "No Member State data for these filters.": "Sem dados de Estados-Membros para estes filtros.", "Northern": "Norte", "Western": "Ocidental", "Central": "Central", "Eastern": "Oriental", "Southern": "Austral", "Search:": "Pesquisar:", "Show": "Mostrar", "entries": "entradas", "Previous": "Anterior", "Next": "Seguinte", "First": "Primeiro", "Last": "\u00daltimo", "No data available in table": "Sem dados dispon\u00edveis na tabela", "No matching records found": "Nenhum registo correspondente", "countries": "pa\u00edses", "total_detections": "dete\u00e7\u00f5es_totais", "human_cases": "casos_humanos", "environmental_detections": "dete\u00e7\u00f5es_ambientais", "records_to_verify": "registos_a_verificar", "latest_issue": "\u00faltima_edi\u00e7\u00e3o", "issue_date": "data_edi\u00e7\u00e3o", "issue_id": "id_edi\u00e7\u00e3o", "country": "pa\u00eds", "rcc": "ccr", "virus_type": "tipo_de_v\u00edrus", "detection_source": "fonte_de_dete\u00e7\u00e3o", "signal_type": "tipo_de_sinal", "cases": "casos", "summary_text": "texto_resumo", "link": "liga\u00e7\u00e3o", "year": "ano", "Year:": "Ano:", "Date:": "Data:", "Publication date:": "Data de publica\u00e7\u00e3o:", "Virus type:": "Tipo de v\u00edrus:", "Detection source:": "Fonte de dete\u00e7\u00e3o:", "Virus:": "V\u00edrus:", "Member State:": "Estado-Membro:", "Overview": "Vis\u00e3o geral", "Downloads": "Transfer\u00eancias", "Records": "Registos"}, "es": {"Trends & quality": "Tendencias y calidad", "Map": "Mapa", "RCC": "CCR", "Member State": "Estado miembro", "Virus type": "Tipo de virus", "Detection source": "Fuente de detecci\u00f3n", "Publication year": "A\u00f1o de publicaci\u00f3n", "GPEI publication date": "Fecha de publicaci\u00f3n GPEI", "Latest": "\u00daltima", "Full history": "Historial completo", "Show RCC layer": "Mostrar capa CCR", "Show polio markers": "Mostrar marcadores de polio", "All": "Todos", "African Poliovirus Epidemic Intelligence Dashboard": "Panel de inteligencia epid\u00e9mica sobre poliovirus en \u00c1frica", "Automated monitoring of publicly reported poliovirus signals in Africa": "Seguimiento automatizado de se\u00f1ales de poliovirus publicadas en \u00c1frica", "Total detections (human + environmental)": "Total de detecciones (humanas + ambientales)", "Affected Member States": "Estados miembros afectados", "RCC represented": "CCR representados", "No dated issue": "Ninguna edici\u00f3n con fecha", "Latest issue date": "Fecha de la \u00faltima edici\u00f3n", "Human detections": "Detecciones humanas", "Environmental detections": "Detecciones ambientales", "Polio Africa map": "Mapa de la polio en \u00c1frica", "RCC and epidemiological profile": "CCR y perfil epidemiol\u00f3gico", "Temporal evolution of detections": "Evoluci\u00f3n temporal de las detecciones", "Operational interpretation": "Interpretaci\u00f3n operativa", "This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI.": "Este panel es un prototipo no oficial. Muestra las se\u00f1ales de polio publicadas por la GPEI (Polio This Week). La agrupaci\u00f3n por CCR se usa solo como esquema de clasificaci\u00f3n geogr\u00e1fica; no es un producto oficial del Africa CDC, la OMS ni la GPEI.", "Alerts are prepared for RCCs; sending is currently limited to the author.": "Las alertas se preparan para los CCR; el env\u00edo est\u00e1 actualmente limitado al autor.", "Affected Member States are shown with polio markers.": "Los Estados miembros afectados se muestran con marcadores de polio.", "The dashboard distinguishes detections in humans and environmental surveillance.": "El panel distingue las detecciones en humanos y en la vigilancia ambiental.", "The same filters drive the map, charts, RCC summaries, country details, and downloadable data.": "Los mismos filtros controlan el mapa, los gr\u00e1ficos, los res\u00famenes por CCR, el detalle por pa\u00eds y los datos descargables.", "RCC summary": "Resumen por CCR", "Country details": "Detalle por pa\u00eds", "Interpretation rule": "Regla de interpretaci\u00f3n", "These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date.": "Son detecciones notificadas por fecha de publicaci\u00f3n de la GPEI, no la incidencia por fecha de inicio de la par\u00e1lisis o de toma de muestra.", "Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.": "Las publicaciones ausentes se tratan como datos no disponibles, nunca como cero. La cobertura hist\u00f3rica var\u00eda seg\u00fan el a\u00f1o.", "Detections reported by GPEI publication date": "Detecciones notificadas por fecha de publicaci\u00f3n de la GPEI", "Archive coverage by year": "Cobertura del archivo por a\u00f1o", "Reported detections by virus type": "Detecciones notificadas por tipo de virus", "Member States with most reported detections": "Estados miembros con m\u00e1s detecciones notificadas", "Historical data quality and coverage": "Calidad y cobertura de los datos hist\u00f3ricos", "Filtered polio records": "Registros de polio filtrados", "Download filtered data": "Descargar los datos filtrados", "Polio records (filtered)": "Registros de polio (filtrados)", "Note": "Nota", "This app uses the real PREIS_Polio_FV input files.": "Esta aplicaci\u00f3n usa los archivos de entrada reales de PREIS_Polio_FV.", "It is aligned with the project RCC mapping and Africa RCC geojson.": "Est\u00e1 alineada con el mapeo de CCR del proyecto y el geojson de los CCR de \u00c1frica.", "The exported file includes virus type and detection source.": "El archivo exportado incluye el tipo de virus y la fuente de detecci\u00f3n.", "count to verify": "recuento por verificar", "None in filtered data": "Ninguno en los datos filtrados", "RCC layer": "Capa CCR", "Polio markers": "Marcadores de polio", "Africa CDC RCC": "CCR del Africa CDC", "Records:": "Registros:", "Virus type(s):": "Tipo(s) de virus:", "Detection source(s):": "Fuente(s) de detecci\u00f3n:", "Detections": "Detecciones", "Reported detections": "Detecciones notificadas", "Available publication dates": "Fechas de publicaci\u00f3n disponibles", "Available GPEI publications; missing dates are not zero": "Publicaciones GPEI disponibles; las fechas ausentes no son cero", "Human": "Humano", "Environmental": "Ambiental", "Both": "Ambos", "Unspecified": "No especificado", "No RCC data available.": "No hay datos de CCR disponibles.", "No virus type data available.": "No hay datos de tipo de virus disponibles.", "No detection source data available.": "No hay datos de fuente de detecci\u00f3n disponibles.", "No dated time series available.": "No hay serie temporal con fechas disponible.", "No historical publication data for these filters.": "No hay datos hist\u00f3ricos de publicaci\u00f3n para estos filtros.", "No coverage data for these filters.": "No hay datos de cobertura para estos filtros.", "No virus data for these filters.": "No hay datos de virus para estos filtros.", "No Member State data for these filters.": "No hay datos de Estados miembros para estos filtros.", "Northern": "Norte", "Western": "Occidental", "Central": "Central", "Eastern": "Oriental", "Southern": "Austral", "Search:": "Buscar:", "Show": "Mostrar", "entries": "entradas", "Previous": "Anterior", "Next": "Siguiente", "First": "Primero", "Last": "\u00daltimo", "No data available in table": "No hay datos disponibles en la tabla", "No matching records found": "No se encontraron registros coincidentes", "countries": "pa\u00edses", "total_detections": "detecciones_totales", "human_cases": "casos_humanos", "environmental_detections": "detecciones_ambientales", "records_to_verify": "registros_por_verificar", "latest_issue": "\u00faltima_edici\u00f3n", "issue_date": "fecha_edici\u00f3n", "issue_id": "id_edici\u00f3n", "country": "pa\u00eds", "rcc": "ccr", "virus_type": "tipo_de_virus", "detection_source": "fuente_de_detecci\u00f3n", "signal_type": "tipo_de_se\u00f1al", "cases": "casos", "summary_text": "texto_resumen", "link": "enlace", "year": "a\u00f1o", "Year:": "A\u00f1o:", "Date:": "Fecha:", "Publication date:": "Fecha de publicaci\u00f3n:", "Virus type:": "Tipo de virus:", "Detection source:": "Fuente de detecci\u00f3n:", "Virus:": "Virus:", "Member State:": "Estado miembro:", "Overview": "Resumen", "Downloads": "Descargas", "Records": "Registros"}, "ar": {"Trends & quality": "\u0627\u0644\u0627\u062a\u062c\u0627\u0647\u0627\u062a \u0648\u0627\u0644\u062c\u0648\u062f\u0629", "Map": "\u0627\u0644\u062e\u0631\u064a\u0637\u0629", "RCC": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a", "Member State": "\u0627\u0644\u062f\u0648\u0644\u0629 \u0627\u0644\u0639\u0636\u0648", "Virus type": "\u0646\u0648\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633", "Detection source": "\u0645\u0635\u062f\u0631 \u0627\u0644\u0643\u0634\u0641", "Publication year": "\u0633\u0646\u0629 \u0627\u0644\u0646\u0634\u0631", "GPEI publication date": "\u062a\u0627\u0631\u064a\u062e \u0646\u0634\u0631 \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629 \u0627\u0644\u0639\u0627\u0644\u0645\u064a\u0629 \u0644\u0627\u0633\u062a\u0626\u0635\u0627\u0644 \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644", "Latest": "\u0627\u0644\u0623\u062d\u062f\u062b", "Full history": "\u0627\u0644\u0633\u062c\u0644 \u0627\u0644\u0643\u0627\u0645\u0644", "Show RCC layer": "\u0625\u0638\u0647\u0627\u0631 \u0637\u0628\u0642\u0629 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629", "Show polio markers": "\u0625\u0638\u0647\u0627\u0631 \u0639\u0644\u0627\u0645\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644", "All": "\u0627\u0644\u0643\u0644", "African Poliovirus Epidemic Intelligence Dashboard": "\u0644\u0648\u062d\u0629 \u0645\u0639\u0644\u0648\u0645\u0627\u062a \u0627\u0644\u0627\u0633\u062a\u062e\u0628\u0627\u0631\u0627\u062a \u0627\u0644\u0648\u0628\u0627\u0626\u064a\u0629 \u0644\u0641\u064a\u0631\u0648\u0633 \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Automated monitoring of publicly reported poliovirus signals in Africa": "\u0631\u0635\u062f \u0622\u0644\u064a \u0644\u0625\u0634\u0627\u0631\u0627\u062a \u0641\u064a\u0631\u0648\u0633 \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 \u0627\u0644\u0645\u0639\u0644\u0646\u0629 \u0639\u0644\u0646\u064b\u0627 \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "Total detections (human + environmental)": "\u0625\u062c\u0645\u0627\u0644\u064a \u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629 (\u0628\u0634\u0631\u064a\u0629 + \u0628\u064a\u0626\u064a\u0629)", "Affected Member States": "\u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621 \u0627\u0644\u0645\u062a\u0623\u062b\u0631\u0629", "RCC represented": "\u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0627\u0644\u0645\u0645\u062b\u0644\u0629", "No dated issue": "\u0644\u0627 \u064a\u0648\u062c\u062f \u0625\u0635\u062f\u0627\u0631 \u0645\u0624\u0631\u062e", "Latest issue date": "\u062a\u0627\u0631\u064a\u062e \u0622\u062e\u0631 \u0625\u0635\u062f\u0627\u0631", "Human detections": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0628\u0634\u0631\u064a\u0629 \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629", "Environmental detections": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0628\u064a\u0626\u064a\u0629 \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629", "Polio Africa map": "\u062e\u0631\u064a\u0637\u0629 \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 \u0641\u064a \u0623\u0641\u0631\u064a\u0642\u064a\u0627", "RCC and epidemiological profile": "\u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0648\u0627\u0644\u0645\u0644\u0641 \u0627\u0644\u0648\u0628\u0627\u0626\u064a", "Temporal evolution of detections": "\u0627\u0644\u062a\u0637\u0648\u0631 \u0627\u0644\u0632\u0645\u0646\u064a \u0644\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629", "Operational interpretation": "\u0627\u0644\u062a\u0641\u0633\u064a\u0631 \u0627\u0644\u062a\u0634\u063a\u064a\u0644\u064a", "This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI.": "\u0647\u0630\u0647 \u0627\u0644\u0644\u0648\u062d\u0629 \u0646\u0645\u0648\u0630\u062c \u0623\u0648\u0644\u064a \u063a\u064a\u0631 \u0631\u0633\u0645\u064a. \u062a\u0639\u0631\u0636 \u0625\u0634\u0627\u0631\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 \u0627\u0644\u062a\u064a \u062a\u0646\u0634\u0631\u0647\u0627 \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629 \u0627\u0644\u0639\u0627\u0644\u0645\u064a\u0629 \u0644\u0627\u0633\u062a\u0626\u0635\u0627\u0644 \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 (Polio This Week). \u064a\u064f\u0633\u062a\u062e\u062f\u0645 \u062a\u062c\u0645\u064a\u0639 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0643\u062a\u0635\u0646\u064a\u0641 \u062c\u063a\u0631\u0627\u0641\u064a \u0641\u0642\u0637\u060c \u0648\u0644\u064a\u0633 \u0645\u0646\u062a\u062c\u064b\u0627 \u0631\u0633\u0645\u064a\u064b\u0627 \u0644\u0645\u0631\u0627\u0643\u0632 \u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u0644\u0645\u0643\u0627\u0641\u062d\u0629 \u0627\u0644\u0623\u0645\u0631\u0627\u0636 \u0623\u0648 \u0645\u0646\u0638\u0645\u0629 \u0627\u0644\u0635\u062d\u0629 \u0627\u0644\u0639\u0627\u0644\u0645\u064a\u0629 \u0623\u0648 \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629.", "Alerts are prepared for RCCs; sending is currently limited to the author.": "\u062a\u064f\u0639\u062f\u0651 \u0627\u0644\u062a\u0646\u0628\u064a\u0647\u0627\u062a \u0644\u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629\u061b \u0648\u0627\u0644\u0625\u0631\u0633\u0627\u0644 \u0645\u062d\u0635\u0648\u0631 \u062d\u0627\u0644\u064a\u064b\u0627 \u0628\u0627\u0644\u0645\u0624\u0644\u0641.", "Affected Member States are shown with polio markers.": "\u062a\u0638\u0647\u0631 \u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621 \u0627\u0644\u0645\u062a\u0623\u062b\u0631\u0629 \u0628\u0639\u0644\u0627\u0645\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644.", "The dashboard distinguishes detections in humans and environmental surveillance.": "\u062a\u0645\u064a\u0651\u0632 \u0627\u0644\u0644\u0648\u062d\u0629 \u0628\u064a\u0646 \u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629 \u0644\u062f\u0649 \u0627\u0644\u0628\u0634\u0631 \u0648\u0641\u064a \u0627\u0644\u0631\u0635\u062f \u0627\u0644\u0628\u064a\u0626\u064a.", "The same filters drive the map, charts, RCC summaries, country details, and downloadable data.": "\u062a\u062a\u062d\u0643\u0645 \u0627\u0644\u0645\u0631\u0634\u062d\u0627\u062a \u0646\u0641\u0633\u0647\u0627 \u0641\u064a \u0627\u0644\u062e\u0631\u064a\u0637\u0629 \u0648\u0627\u0644\u0631\u0633\u0648\u0645 \u0627\u0644\u0628\u064a\u0627\u0646\u064a\u0629 \u0648\u0645\u0644\u062e\u0635\u0627\u062a \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0648\u062a\u0641\u0627\u0635\u064a\u0644 \u0627\u0644\u062f\u0648\u0644 \u0648\u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0642\u0627\u0628\u0644\u0629 \u0644\u0644\u062a\u0646\u0632\u064a\u0644.", "RCC summary": "\u0645\u0644\u062e\u0635 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629", "Country details": "\u062a\u0641\u0627\u0635\u064a\u0644 \u0627\u0644\u062f\u0648\u0644", "Interpretation rule": "\u0642\u0627\u0639\u062f\u0629 \u0627\u0644\u062a\u0641\u0633\u064a\u0631", "These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date.": "\u0647\u0630\u0647 \u062d\u0627\u0644\u0627\u062a \u0645\u0643\u062a\u0634\u0641\u0629 \u0645\u064f\u0628\u0644\u064e\u0651\u063a \u0639\u0646\u0647\u0627 \u0628\u062d\u0633\u0628 \u062a\u0627\u0631\u064a\u062e \u0646\u0634\u0631 \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629\u060c \u0648\u0644\u064a\u0633\u062a \u0645\u0639\u062f\u0644 \u0627\u0644\u0625\u0635\u0627\u0628\u0629 \u0628\u062d\u0633\u0628 \u062a\u0627\u0631\u064a\u062e \u0628\u062f\u0621 \u0627\u0644\u0634\u0644\u0644 \u0623\u0648 \u062c\u0645\u0639 \u0627\u0644\u0639\u064a\u0646\u0629.", "Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.": "\u062a\u064f\u0639\u0627\u0645\u064e\u0644 \u0627\u0644\u0625\u0635\u062f\u0627\u0631\u0627\u062a \u0627\u0644\u0645\u0641\u0642\u0648\u062f\u0629 \u0639\u0644\u0649 \u0623\u0646\u0647\u0627 \u0628\u064a\u0627\u0646\u0627\u062a \u063a\u064a\u0631 \u0645\u062a\u0627\u062d\u0629 \u0648\u0644\u064a\u0633\u062a \u0635\u0641\u0631\u064b\u0627 \u0623\u0628\u062f\u064b\u0627. \u0648\u062a\u062e\u062a\u0644\u0641 \u0627\u0644\u062a\u063a\u0637\u064a\u0629 \u0627\u0644\u062a\u0627\u0631\u064a\u062e\u064a\u0629 \u0645\u0646 \u0633\u0646\u0629 \u0625\u0644\u0649 \u0623\u062e\u0631\u0649.", "Detections reported by GPEI publication date": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629 \u0628\u062d\u0633\u0628 \u062a\u0627\u0631\u064a\u062e \u0646\u0634\u0631 \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629", "Archive coverage by year": "\u062a\u063a\u0637\u064a\u0629 \u0627\u0644\u0623\u0631\u0634\u064a\u0641 \u062d\u0633\u0628 \u0627\u0644\u0633\u0646\u0629", "Reported detections by virus type": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629 \u0627\u0644\u0645\u064f\u0628\u0644\u064e\u0651\u063a \u0639\u0646\u0647\u0627 \u0628\u062d\u0633\u0628 \u0646\u0648\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633", "Member States with most reported detections": "\u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621 \u0627\u0644\u0623\u0643\u062b\u0631 \u0625\u0628\u0644\u0627\u063a\u064b\u0627 \u0639\u0646 \u062d\u0627\u0644\u0627\u062a \u0645\u0643\u062a\u0634\u0641\u0629", "Historical data quality and coverage": "\u062c\u0648\u062f\u0629 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u062a\u0627\u0631\u064a\u062e\u064a\u0629 \u0648\u062a\u063a\u0637\u064a\u062a\u0647\u0627", "Filtered polio records": "\u0633\u062c\u0644\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 \u0628\u0639\u062f \u0627\u0644\u062a\u0635\u0641\u064a\u0629", "Download filtered data": "\u062a\u0646\u0632\u064a\u0644 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u0635\u0641\u0651\u0627\u0629", "Polio records (filtered)": "\u0633\u062c\u0644\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644 (\u0645\u0635\u0641\u0651\u0627\u0629)", "Note": "\u0645\u0644\u0627\u062d\u0638\u0629", "This app uses the real PREIS_Polio_FV input files.": "\u064a\u0633\u062a\u062e\u062f\u0645 \u0647\u0630\u0627 \u0627\u0644\u062a\u0637\u0628\u064a\u0642 \u0645\u0644\u0641\u0627\u062a \u0627\u0644\u0625\u062f\u062e\u0627\u0644 \u0627\u0644\u0641\u0639\u0644\u064a\u0629 \u0644\u0645\u0634\u0631\u0648\u0639 PREIS_Polio_FV.", "It is aligned with the project RCC mapping and Africa RCC geojson.": "\u0648\u0647\u0648 \u0645\u062a\u0648\u0627\u0641\u0642 \u0645\u0639 \u062e\u0631\u064a\u0637\u0629 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0644\u0644\u0645\u0634\u0631\u0648\u0639 \u0648\u0645\u0644\u0641 geojson \u0627\u0644\u062e\u0627\u0635 \u0628\u0647\u0627.", "The exported file includes virus type and detection source.": "\u064a\u062a\u0636\u0645\u0646 \u0627\u0644\u0645\u0644\u0641 \u0627\u0644\u0645\u0635\u062f\u064e\u0651\u0631 \u0646\u0648\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633 \u0648\u0645\u0635\u062f\u0631 \u0627\u0644\u0643\u0634\u0641.", "count to verify": "\u0627\u0644\u0639\u062f\u062f \u0642\u064a\u062f \u0627\u0644\u062a\u062d\u0642\u0642", "None in filtered data": "\u0644\u0627 \u0634\u064a\u0621 \u0641\u064a \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0645\u0635\u0641\u0651\u0627\u0629", "RCC layer": "\u0637\u0628\u0642\u0629 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629", "Polio markers": "\u0639\u0644\u0627\u0645\u0627\u062a \u0634\u0644\u0644 \u0627\u0644\u0623\u0637\u0641\u0627\u0644", "Africa CDC RCC": "\u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629 \u0644\u0645\u0631\u0627\u0643\u0632 \u0623\u0641\u0631\u064a\u0642\u064a\u0627 \u0644\u0645\u0643\u0627\u0641\u062d\u0629 \u0627\u0644\u0623\u0645\u0631\u0627\u0636", "Records:": "\u0627\u0644\u0633\u062c\u0644\u0627\u062a:", "Virus type(s):": "\u0646\u0648\u0639/\u0623\u0646\u0648\u0627\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633:", "Detection source(s):": "\u0645\u0635\u062f\u0631/\u0645\u0635\u0627\u062f\u0631 \u0627\u0644\u0643\u0634\u0641:", "Detections": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629", "Reported detections": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a \u0627\u0644\u0645\u0643\u062a\u0634\u0641\u0629 \u0627\u0644\u0645\u064f\u0628\u0644\u064e\u0651\u063a \u0639\u0646\u0647\u0627", "Available publication dates": "\u062a\u0648\u0627\u0631\u064a\u062e \u0627\u0644\u0646\u0634\u0631 \u0627\u0644\u0645\u062a\u0627\u062d\u0629", "Available GPEI publications; missing dates are not zero": "\u0625\u0635\u062f\u0627\u0631\u0627\u062a \u0627\u0644\u0645\u0628\u0627\u062f\u0631\u0629 \u0627\u0644\u0645\u062a\u0627\u062d\u0629\u061b \u0627\u0644\u062a\u0648\u0627\u0631\u064a\u062e \u0627\u0644\u0645\u0641\u0642\u0648\u062f\u0629 \u0644\u064a\u0633\u062a \u0635\u0641\u0631\u064b\u0627", "Human": "\u0628\u0634\u0631\u064a", "Environmental": "\u0628\u064a\u0626\u064a", "Both": "\u0643\u0644\u0627\u0647\u0645\u0627", "Unspecified": "\u063a\u064a\u0631 \u0645\u062d\u062f\u062f", "No RCC data available.": "\u0644\u0627 \u062a\u062a\u0648\u0641\u0631 \u0628\u064a\u0627\u0646\u0627\u062a \u0639\u0646 \u0645\u0631\u0627\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629.", "No virus type data available.": "\u0644\u0627 \u062a\u062a\u0648\u0641\u0631 \u0628\u064a\u0627\u0646\u0627\u062a \u0639\u0646 \u0646\u0648\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633.", "No detection source data available.": "\u0644\u0627 \u062a\u062a\u0648\u0641\u0631 \u0628\u064a\u0627\u0646\u0627\u062a \u0639\u0646 \u0645\u0635\u062f\u0631 \u0627\u0644\u0643\u0634\u0641.", "No dated time series available.": "\u0644\u0627 \u062a\u062a\u0648\u0641\u0631 \u0633\u0644\u0633\u0644\u0629 \u0632\u0645\u0646\u064a\u0629 \u0645\u0624\u0631\u062e\u0629.", "No historical publication data for these filters.": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0646\u0634\u0631 \u062a\u0627\u0631\u064a\u062e\u064a\u0629 \u0644\u0647\u0630\u0647 \u0627\u0644\u0645\u0631\u0634\u062d\u0627\u062a.", "No coverage data for these filters.": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u062a\u063a\u0637\u064a\u0629 \u0644\u0647\u0630\u0647 \u0627\u0644\u0645\u0631\u0634\u062d\u0627\u062a.", "No virus data for these filters.": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0641\u064a\u0631\u0648\u0633\u0627\u062a \u0644\u0647\u0630\u0647 \u0627\u0644\u0645\u0631\u0634\u062d\u0627\u062a.", "No Member State data for these filters.": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0644\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621 \u0644\u0647\u0630\u0647 \u0627\u0644\u0645\u0631\u0634\u062d\u0627\u062a.", "Northern": "\u0627\u0644\u0634\u0645\u0627\u0644", "Western": "\u0627\u0644\u063a\u0631\u0628", "Central": "\u0627\u0644\u0648\u0633\u0637", "Eastern": "\u0627\u0644\u0634\u0631\u0642", "Southern": "\u0627\u0644\u062c\u0646\u0648\u0628", "Search:": "\u0628\u062d\u062b:", "Show": "\u0639\u0631\u0636", "entries": "\u0625\u062f\u062e\u0627\u0644\u0627\u062a", "Previous": "\u0627\u0644\u0633\u0627\u0628\u0642", "Next": "\u0627\u0644\u062a\u0627\u0644\u064a", "First": "\u0627\u0644\u0623\u0648\u0644", "Last": "\u0627\u0644\u0623\u062e\u064a\u0631", "No data available in table": "\u0644\u0627 \u062a\u0648\u062c\u062f \u0628\u064a\u0627\u0646\u0627\u062a \u0641\u064a \u0627\u0644\u062c\u062f\u0648\u0644", "No matching records found": "\u0644\u0645 \u064a\u062a\u0645 \u0627\u0644\u0639\u062b\u0648\u0631 \u0639\u0644\u0649 \u0633\u062c\u0644\u0627\u062a \u0645\u0637\u0627\u0628\u0642\u0629", "countries": "\u0627\u0644\u062f\u0648\u0644", "total_detections": "\u0625\u062c\u0645\u0627\u0644\u064a_\u0627\u0644\u062d\u0627\u0644\u0627\u062a", "human_cases": "\u062d\u0627\u0644\u0627\u062a_\u0628\u0634\u0631\u064a\u0629", "environmental_detections": "\u062d\u0627\u0644\u0627\u062a_\u0628\u064a\u0626\u064a\u0629", "records_to_verify": "\u0633\u062c\u0644\u0627\u062a_\u0642\u064a\u062f_\u0627\u0644\u062a\u062d\u0642\u0642", "latest_issue": "\u0622\u062e\u0631_\u0625\u0635\u062f\u0627\u0631", "issue_date": "\u062a\u0627\u0631\u064a\u062e_\u0627\u0644\u0625\u0635\u062f\u0627\u0631", "issue_id": "\u0645\u0639\u0631\u0651\u0641_\u0627\u0644\u0625\u0635\u062f\u0627\u0631", "country": "\u0627\u0644\u062f\u0648\u0644\u0629", "rcc": "\u0627\u0644\u0645\u0631\u0643\u0632", "virus_type": "\u0646\u0648\u0639_\u0627\u0644\u0641\u064a\u0631\u0648\u0633", "detection_source": "\u0645\u0635\u062f\u0631_\u0627\u0644\u0643\u0634\u0641", "signal_type": "\u0646\u0648\u0639_\u0627\u0644\u0625\u0634\u0627\u0631\u0629", "cases": "\u0627\u0644\u062d\u0627\u0644\u0627\u062a", "summary_text": "\u0646\u0635_\u0627\u0644\u0645\u0644\u062e\u0635", "link": "\u0631\u0627\u0628\u0637", "year": "\u0627\u0644\u0633\u0646\u0629", "Year:": "\u0627\u0644\u0633\u0646\u0629:", "Date:": "\u0627\u0644\u062a\u0627\u0631\u064a\u062e:", "Publication date:": "\u062a\u0627\u0631\u064a\u062e \u0627\u0644\u0646\u0634\u0631:", "Virus type:": "\u0646\u0648\u0639 \u0627\u0644\u0641\u064a\u0631\u0648\u0633:", "Detection source:": "\u0645\u0635\u062f\u0631 \u0627\u0644\u0643\u0634\u0641:", "Virus:": "\u0627\u0644\u0641\u064a\u0631\u0648\u0633:", "Member State:": "\u0627\u0644\u062f\u0648\u0644\u0629 \u0627\u0644\u0639\u0636\u0648:", "Overview": "\u0646\u0638\u0631\u0629 \u0639\u0627\u0645\u0629", "Downloads": "\u0627\u0644\u062a\u0646\u0632\u064a\u0644\u0627\u062a", "Records": "\u0627\u0644\u0633\u062c\u0644\u0627\u062a"}, "sw": {"Trends & quality": "Mielekeo na ubora", "Map": "Ramani", "RCC": "RCC", "Member State": "Nchi Mwanachama", "Virus type": "Aina ya virusi", "Detection source": "Chanzo cha ugunduzi", "Publication year": "Mwaka wa uchapishaji", "GPEI publication date": "Tarehe ya uchapishaji wa GPEI", "Latest": "Ya hivi karibuni", "Full history": "Historia kamili", "Show RCC layer": "Onyesha safu ya RCC", "Show polio markers": "Onyesha alama za polio", "All": "Zote", "African Poliovirus Epidemic Intelligence Dashboard": "Dashibodi ya taarifa za mlipuko wa virusi vya polio barani Afrika", "Automated monitoring of publicly reported poliovirus signals in Africa": "Ufuatiliaji wa kiotomatiki wa ishara za virusi vya polio zilizochapishwa barani Afrika", "Total detections (human + environmental)": "Jumla ya ugunduzi (binadamu + mazingira)", "Affected Member States": "Nchi Wanachama zilizoathirika", "RCC represented": "RCC zilizowakilishwa", "No dated issue": "Hakuna toleo lenye tarehe", "Latest issue date": "Tarehe ya toleo la hivi karibuni", "Human detections": "Ugunduzi kwa binadamu", "Environmental detections": "Ugunduzi wa mazingira", "Polio Africa map": "Ramani ya polio barani Afrika", "RCC and epidemiological profile": "RCC na wasifu wa epidemiolojia", "Temporal evolution of detections": "Mabadiliko ya ugunduzi kwa wakati", "Operational interpretation": "Ufafanuzi wa kiutendaji", "This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI.": "Dashibodi hii ni mfano usio rasmi. Inaonyesha ishara za polio zilizochapishwa na GPEI (Polio This Week). Mgawanyo wa RCC hutumika tu kama mpango wa uainishaji wa kijiografia; si bidhaa rasmi ya Africa CDC, WHO wala GPEI.", "Alerts are prepared for RCCs; sending is currently limited to the author.": "Tahadhari huandaliwa kwa RCC; kutuma kunawekewa kikomo kwa mwandishi kwa sasa.", "Affected Member States are shown with polio markers.": "Nchi Wanachama zilizoathirika zinaonyeshwa kwa alama za polio.", "The dashboard distinguishes detections in humans and environmental surveillance.": "Dashibodi inatofautisha ugunduzi kwa binadamu na ule wa ufuatiliaji wa mazingira.", "The same filters drive the map, charts, RCC summaries, country details, and downloadable data.": "Vichujio hivyo hivyo vinadhibiti ramani, michoro, muhtasari wa RCC, maelezo ya nchi na data inayoweza kupakuliwa.", "RCC summary": "Muhtasari wa RCC", "Country details": "Maelezo ya nchi", "Interpretation rule": "Kanuni ya ufafanuzi", "These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date.": "Haya ni ugunduzi ulioripotiwa kwa tarehe ya uchapishaji wa GPEI, si kiwango cha maambukizi kwa tarehe ya kuanza kwa kupooza au ya kukusanya sampuli.", "Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.": "Machapisho yanayokosekana huchukuliwa kama data isiyopatikana, kamwe si sifuri. Ufunikaji wa kihistoria hutofautiana kwa mwaka.", "Detections reported by GPEI publication date": "Ugunduzi ulioripotiwa kwa tarehe ya uchapishaji wa GPEI", "Archive coverage by year": "Ufunikaji wa kumbukumbu kwa mwaka", "Reported detections by virus type": "Ugunduzi ulioripotiwa kwa aina ya virusi", "Member States with most reported detections": "Nchi Wanachama zenye ugunduzi mwingi zaidi ulioripotiwa", "Historical data quality and coverage": "Ubora na ufunikaji wa data ya kihistoria", "Filtered polio records": "Rekodi za polio zilizochujwa", "Download filtered data": "Pakua data iliyochujwa", "Polio records (filtered)": "Rekodi za polio (zilizochujwa)", "Note": "Kumbuka", "This app uses the real PREIS_Polio_FV input files.": "Programu hii hutumia faili halisi za pembejeo za PREIS_Polio_FV.", "It is aligned with the project RCC mapping and Africa RCC geojson.": "Inaendana na ramani ya RCC ya mradi na geojson ya RCC za Afrika.", "The exported file includes virus type and detection source.": "Faili iliyosafirishwa inajumuisha aina ya virusi na chanzo cha ugunduzi.", "count to verify": "hesabu inasubiri uthibitisho", "None in filtered data": "Hakuna katika data iliyochujwa", "RCC layer": "Safu ya RCC", "Polio markers": "Alama za polio", "Africa CDC RCC": "RCC za Africa CDC", "Records:": "Rekodi:", "Virus type(s):": "Aina za virusi:", "Detection source(s):": "Vyanzo vya ugunduzi:", "Detections": "Ugunduzi", "Reported detections": "Ugunduzi ulioripotiwa", "Available publication dates": "Tarehe za uchapishaji zinazopatikana", "Available GPEI publications; missing dates are not zero": "Machapisho ya GPEI yanayopatikana; tarehe zinazokosekana si sifuri", "Human": "Binadamu", "Environmental": "Mazingira", "Both": "Vyote viwili", "Unspecified": "Haijabainishwa", "No RCC data available.": "Hakuna data ya RCC inayopatikana.", "No virus type data available.": "Hakuna data ya aina ya virusi inayopatikana.", "No detection source data available.": "Hakuna data ya chanzo cha ugunduzi inayopatikana.", "No dated time series available.": "Hakuna mfululizo wa wakati wenye tarehe unaopatikana.", "No historical publication data for these filters.": "Hakuna data ya kihistoria ya uchapishaji kwa vichujio hivi.", "No coverage data for these filters.": "Hakuna data ya ufunikaji kwa vichujio hivi.", "No virus data for these filters.": "Hakuna data ya virusi kwa vichujio hivi.", "No Member State data for these filters.": "Hakuna data ya Nchi Wanachama kwa vichujio hivi.", "Northern": "Kaskazini", "Western": "Magharibi", "Central": "Kati", "Eastern": "Mashariki", "Southern": "Kusini", "Search:": "Tafuta:", "Show": "Onyesha", "entries": "maingizo", "Previous": "Iliyotangulia", "Next": "Inayofuata", "First": "Ya kwanza", "Last": "Ya mwisho", "No data available in table": "Hakuna data kwenye jedwali", "No matching records found": "Hakuna rekodi zinazolingana", "countries": "nchi", "total_detections": "jumla_ya_ugunduzi", "human_cases": "visa_vya_binadamu", "environmental_detections": "ugunduzi_wa_mazingira", "records_to_verify": "rekodi_za_kuthibitisha", "latest_issue": "toleo_la_hivi_karibuni", "issue_date": "tarehe_ya_toleo", "issue_id": "kitambulisho_cha_toleo", "country": "nchi", "rcc": "rcc", "virus_type": "aina_ya_virusi", "detection_source": "chanzo_cha_ugunduzi", "signal_type": "aina_ya_ishara", "cases": "visa", "summary_text": "maandishi_ya_muhtasari", "link": "kiungo", "year": "mwaka", "Year:": "Mwaka:", "Date:": "Tarehe:", "Publication date:": "Tarehe ya uchapishaji:", "Virus type:": "Aina ya virusi:", "Detection source:": "Chanzo cha ugunduzi:", "Virus:": "Virusi:", "Member State:": "Nchi Mwanachama:", "Overview": "Muhtasari", "Downloads": "Vipakuliwa", "Records": "Rekodi"}};
  var MO = {}, MT = {}, MS = {}, NOTE = {"pt": "Tradu\u00e7\u00e3o provis\u00f3ria, a rever por falantes nativos.", "es": "Traducci\u00f3n provisional, pendiente de revisi\u00f3n por hablantes nativos.", "ar": "\u062a\u0631\u062c\u0645\u0629 \u0623\u0648\u0644\u064a\u0629 \u0628\u0627\u0646\u062a\u0638\u0627\u0631 \u0645\u0631\u0627\u062c\u0639\u0629 \u0646\u0627\u0637\u0642\u064a\u0646 \u0623\u0635\u0644\u064a\u064a\u0646.", "sw": "Tafsiri ya awali, inasubiri mapitio ya wazungumzaji asilia."};
  var PR = [
  [/^Showing (\S+) to (\S+) of (\S+) entries \(filtered from (\S+) total entries\)$/, 'info_f', {"fr": "Affichage de $1 \u00e0 $2 sur $3 entr\u00e9es (filtr\u00e9es sur un total de $4)", "pt": "Mostrando $1 a $2 de $3 entradas (filtradas de um total de $4)", "es": "Mostrando $1 a $2 de $3 entradas (filtrado de un total de $4)", "ar": "\u0639\u0631\u0636 $1 \u0625\u0644\u0649 $2 \u0645\u0646 $3 \u0625\u062f\u062e\u0627\u0644\u0627\u064b (\u062a\u0645\u062a \u0627\u0644\u062a\u0635\u0641\u064a\u0629 \u0645\u0646 \u0625\u062c\u0645\u0627\u0644\u064a $4)", "sw": "Inaonyesha $1 hadi $2 kati ya maingizo $3 (yamechujwa kutoka jumla ya $4)"}],
  [/^Showing (\S+) to (\S+) of (\S+) entries$/, 'info', {"fr": "Affichage de $1 \u00e0 $2 sur $3 entr\u00e9es", "pt": "Mostrando $1 a $2 de $3 entradas", "es": "Mostrando $1 a $2 de $3 entradas", "ar": "\u0639\u0631\u0636 $1 \u0625\u0644\u0649 $2 \u0645\u0646 $3 \u0625\u062f\u062e\u0627\u0644\u0627\u064b", "sw": "Inaonyesha $1 hadi $2 kati ya maingizo $3"}],
  [/^RCC: (.+)$/, 'rcc', {"fr": "CCR : $1", "pt": "CCR: $1", "es": "CCR: $1", "ar": "\u0645\u0631\u0643\u0632 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a: $1", "sw": "RCC: $1"}],
  [/^(\d+) \(\+ (\d+) record\(s\) to verify\)$/, 'tv', {"fr": "$1 (+ $2 enregistrement(s) \u00e0 v\u00e9rifier)", "pt": "$1 (+ $2 registo(s) a verificar)", "es": "$1 (+ $2 registro(s) por verificar)", "ar": "$1 (+ $2 \u0633\u062c\u0644 \u0642\u064a\u062f \u0627\u0644\u062a\u062d\u0642\u0642)", "sw": "$1 (+ rekodi $2 za kuthibitisha)"}]
  ];
  var lang = 'en', rec = new WeakMap(), pending = false;
  function has(o,k){ return Object.prototype.hasOwnProperty.call(o,k); }
  function tr1(s){
    var D = L[lang]; if (!D) return s;
    var m = s.match(/^(\s*)([\s\S]*?)(\s*)$/), core = m[2];
    if (!core) return s;
    if (has(D,core)) return m[1]+D[core]+m[3];
    for (var i=0;i<PR.length;i++){
      var p = PR[i], mm = p[0].exec(core);
      if (!mm) continue;
      if (p[1] === 'month'){ var mon = (MO[lang]||{})[mm[1]] || mm[1]; return m[1]+mon+' '+mm[2]+MS[lang]+mm[3]+MT[lang]+mm[4]+m[3]; }
      if (p[2][lang] !== undefined) return m[1]+p[2][lang].replace(/\$(\d)/g, function(_,i){ var g = mm[+i]; return (g === undefined) ? '' : (has(D,g) ? D[g] : g); })+m[3];
    }
    var parts = core.replace(/([.!?])\s+/g,'$1\u0001').split('\u0001');
    if (parts.length > 1){
      var hit = false;
      var out = parts.map(function(q){ if (has(D,q)){ hit = true; return D[q]; } return q; });
      if (hit) return m[1]+out.join(' ')+m[3];
    }
    if (/;|\s\|\s/.test(core) && core.length < 400){
      var sp = core.split(/(\s*;\s*|\s+\|\s+)/), h2 = false;
      var o2 = sp.map(function(q,ix){ if (ix%2===1) return q; if (has(D,q)){ h2 = true; return D[q]; } return q; });
      if (h2) return m[1]+o2.join('')+m[3];
    }
    return s;
  }
  function visit(n){
    var v = n.nodeValue, r = rec.get(n), t;
    if (r && v === r.t){
      t = (lang !== 'en') ? tr1(r.o) : r.o;
      if (t !== v){ n.nodeValue = t; r.t = t; }
      return;
    }
    t = (lang !== 'en') ? tr1(v) : v;
    rec.set(n,{o:v,t:t});
    if (t !== v) n.nodeValue = t;
  }
  function walk(){
    pending = false;
    if (!document.body) return;
    var w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null), n, list = [];
    while ((n = w.nextNode())){
      var p = n.parentNode;
      if (!p || /^(SCRIPT|STYLE|TEXTAREA)$/.test(p.nodeName)) continue;
      if (p.closest && p.closest('.no-tr')) continue;
      list.push(n);
    }
    for (var i=0;i<list.length;i++) visit(list[i]);
  }
  function schedule(){ if (!pending){ pending = true; (window.requestAnimationFrame || setTimeout)(walk); } }
  function setLang(l){
    lang = (L[l] ? l : 'en');
    try { document.documentElement.lang = lang; document.documentElement.dir = 'ltr'; } catch(e){}
    var el = document.getElementById('lang-draft'); if (el) el.textContent = NOTE[lang] || '';
    schedule();
  }
  new MutationObserver(schedule).observe(document.documentElement, {childList:true, subtree:true, characterData:true});
  if (window.jQuery){
    jQuery(document).on('shiny:inputchanged', function(e){ if (e.name === 'ui_lang') setLang(e.value); });
    jQuery(document).on('shiny:connected', function(){ var c = jQuery('input[name=ui_lang]:checked').val(); if (c) setLang(c); });
  }
})();
)---"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = header_title,
    titleWidth = 300,
    tags$li(
      class = "dropdown preis-permanent-title",
      div(
        class = "preis-permanent-title-content",
        tags$div(
          "African Poliovirus Epidemic Intelligence Dashboard",
          class = "preis-permanent-main-title"
        ),
        tags$div(
          "Automated monitoring of publicly reported poliovirus signals in Africa",
          tags$span(
            "Prototype non officiel. Source : GPEI, Polio This Week.",
            style = paste0("display:inline-block;margin-left:10px;padding:1px 8px;border-radius:3px;",
                           "background:#fff3cd;color:#664d03;font-weight:700;")
          ),
          class = "preis-permanent-subtitle"
        )
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      selected = "overview",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends & quality", tabName = "trends", icon = icon("chart-line")),
      menuItem("Map", tabName = "map", icon = icon("globe-africa")),
      menuItem("Records", tabName = "records", icon = icon("table")),
      menuItem("Downloads", tabName = "downloads", icon = icon("download"))
    ),
    br(),
    tags$style(HTML(".no-tr label{color:#ddd; font-weight:600;} .no-tr .shiny-options-group{margin-left:10px;} #lang-draft{padding:0 2px 6px 2px; font-size:11px; color:#f0ad4e;} html[lang='ar'] .content-wrapper, html[lang='ar'] .sidebar-menu {direction:rtl; text-align:right;}")),
    tags$div(class = "no-tr", style = "padding:0 10px;",
             radioButtons("ui_lang", label = NULL,
                          choices = c("English" = "en", "Fran\u00e7ais" = "fr", "Portugu\u00eas" = "pt", "Espa\u00f1ol" = "es", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629" = "ar", "Kiswahili" = "sw"),
                          selected = "en", inline = TRUE),
             tags$div(id = "lang-draft")),
    selectInput("rcc", "RCC", choices = c("All", rcc_levels), selected = "All"),
    selectInput("country", "Member State", choices = c("All", country_levels), selected = "All"),
    selectInput("virus_type", "Virus type", choices = c("All", virus_levels), selected = "All"),
    selectInput("detection_source", "Detection source", choices = c("All", detect_levels), selected = "All"),
    sliderInput(
      "year_range", "Publication year",
      min = min(year_levels), max = max(year_levels),
      value = c(latest_year, latest_year),
      step = 1, sep = ""
    ),
    dateRangeInput("date_range", "GPEI publication date", start = date_max, end = date_max),
    fluidRow(
      column(6, actionButton("reset_filters", "Latest", icon = icon("clock"), width = "100%")),
      column(6, actionButton("show_history", "Full history", icon = icon("history"), width = "100%"))
    ),
    checkboxInput("show_rcc_layer", "Show RCC layer", value = TRUE),
    checkboxInput("show_case_markers", "Show polio markers", value = TRUE)
  ),
  dashboardBody(
    tags$head(
      tags$title("PREIS Polio Tracker | Prototype non officiel"),
      tags$script(HTML(PREIS_I18N_JS)),

      tags$style(HTML("
        /* PREIS_KPI_CARDS_AESTHETIC_FINAL */

        /* Cartes KPI uniformes et compactes */
        body .content-wrapper .small-box {
          height: 190px !important;
          min-height: 190px !important;
          margin-bottom: 20px !important;
          overflow: hidden !important;
          border-radius: 14px !important;
          box-shadow: 0 5px 14px rgba(31, 45, 61, 0.12) !important;
        }

        body .content-wrapper .small-box > .inner {
          position: relative !important;
          z-index: 2 !important;
          height: 190px !important;
          min-height: 190px !important;
          padding: 20px 58px 18px 20px !important;
          box-sizing: border-box !important;

          display: flex !important;
          flex-direction: column !important;
          align-items: flex-start !important;
          justify-content: flex-start !important;

          overflow: hidden !important;
        }

        /* Valeur principale */
        body .content-wrapper .small-box h3 {
          display: block !important;
          width: 100% !important;
          max-width: 100% !important;
          min-height: 56px !important;
          margin: 0 0 14px 0 !important;
          padding: 0 !important;

          color: white !important;
          font-size: 36px !important;
          line-height: 1.08 !important;
          font-weight: 800 !important;
          text-align: left !important;

          white-space: normal !important;
          word-break: keep-all !important;
          overflow-wrap: normal !important;
          hyphens: none !important;
          overflow: hidden !important;
          text-overflow: clip !important;
        }

        /* Date : lisible sur deux lignes au maximum */
        body .content-wrapper #vb_issue .small-box h3,
        body .content-wrapper #vb_issue h3 {
          font-size: 29px !important;
          line-height: 1.08 !important;
          min-height: 64px !important;
          margin-bottom: 8px !important;
        }

        /* Libellés : mots entiers, maximum trois lignes */
        body .content-wrapper .small-box p {
          display: -webkit-box !important;
          width: 100% !important;
          max-width: 100% !important;
          margin: 0 !important;
          padding: 0 !important;

          color: white !important;
          font-size: 17px !important;
          line-height: 1.28 !important;
          font-weight: 500 !important;
          text-align: left !important;

          white-space: normal !important;
          word-break: keep-all !important;
          overflow-wrap: normal !important;
          hyphens: none !important;

          -webkit-box-orient: vertical !important;
          -webkit-line-clamp: 4 !important;
          overflow: hidden !important;
        }

        /* Icônes plus petites et réellement décoratives */
        body .content-wrapper .small-box .icon {
          position: absolute !important;
          top: auto !important;
          right: 10px !important;
          bottom: 8px !important;
          z-index: 1 !important;

          font-size: 64px !important;
          opacity: 0.14 !important;
          pointer-events: none !important;
        }

        body .content-wrapper .small-box .icon i {
          font-size: 64px !important;
        }

        /* Écrans moyens : trois cartes par ligne si nécessaire */
        @media (max-width: 1250px) {
          body .content-wrapper .small-box {
            height: 175px !important;
            min-height: 175px !important;
          }

          body .content-wrapper .small-box > .inner {
            height: 175px !important;
            min-height: 175px !important;
            padding: 18px 54px 16px 18px !important;
          }

          body .content-wrapper .small-box h3 {
            font-size: 32px !important;
            min-height: 48px !important;
            margin-bottom: 10px !important;
          }

          body .content-wrapper #vb_issue .small-box h3,
          body .content-wrapper #vb_issue h3 {
            font-size: 25px !important;
            min-height: 56px !important;
          }

          body .content-wrapper .small-box p {
            font-size: 16px !important;
          }

          body .content-wrapper .small-box .icon,
          body .content-wrapper .small-box .icon i {
            font-size: 56px !important;
          }
        }

        /* Téléphone */
        @media (max-width: 767px) {
          body .content-wrapper .small-box,
          body .content-wrapper .small-box > .inner {
            height: 155px !important;
            min-height: 155px !important;
          }

          body .content-wrapper .small-box h3 {
            font-size: 30px !important;
            min-height: 42px !important;
          }

          body .content-wrapper #vb_issue .small-box h3,
          body .content-wrapper #vb_issue h3 {
            font-size: 23px !important;
          }

          body .content-wrapper .small-box p {
            font-size: 15px !important;
          }
        }
      ")),

      tags$style(HTML("
        /* PREIS_CARD_TEXT_CONTAINMENT_FINAL */

        /* La carte s'agrandit si le texte occupe deux lignes */
        .small-box {
          min-height: 190px !important;
          height: auto !important;
          overflow: hidden !important;
          border-radius: 15px !important;
        }

        /* Réserver de la place pour l'icône à droite */
        .small-box > .inner {
          min-height: 190px !important;
          height: auto !important;
          padding: 18px 72px 18px 16px !important;
          box-sizing: border-box !important;
          overflow: visible !important;
        }

        /* Valeur principale : jamais hors du cadre */
        .small-box h3 {
          display: block !important;
          width: 100% !important;
          max-width: 100% !important;
          margin: 0 0 18px 0 !important;

          font-size: clamp(25px, 2.1vw, 40px) !important;
          line-height: 1.08 !important;
          font-weight: 800 !important;

          white-space: normal !important;
          overflow-wrap: anywhere !important;
          word-break: normal !important;
          overflow: visible !important;
          text-overflow: clip !important;
        }

        /* Libellé descriptif sous la valeur */
        .small-box p {
          display: block !important;
          width: 100% !important;
          max-width: 100% !important;
          margin: 0 !important;

          font-size: clamp(14px, 1.15vw, 20px) !important;
          line-height: 1.32 !important;

          white-space: normal !important;
          overflow-wrap: break-word !important;
          word-break: normal !important;
          overflow: visible !important;
        }

        /* Traitement particulier de la valeur No dated issue */
        #vb_issue .small-box h3,
        #vb_issue h3 {
          font-size: clamp(20px, 1.65vw, 30px) !important;
          line-height: 1.12 !important;
          white-space: normal !important;
          overflow-wrap: break-word !important;
        }

        /* L'icône reste derrière le texte */
        .small-box .icon {
          right: 10px !important;
          top: 38px !important;
          font-size: 70px !important;
          opacity: 0.12 !important;
          pointer-events: none !important;
        }

        @media (max-width: 1200px) {
          .small-box,
          .small-box > .inner {
            min-height: 180px !important;
          }

          .small-box > .inner {
            padding: 16px 58px 16px 14px !important;
          }

          .small-box h3 {
            font-size: 27px !important;
            margin-bottom: 14px !important;
          }

          #vb_issue .small-box h3,
          #vb_issue h3 {
            font-size: 21px !important;
          }

          .small-box p {
            font-size: 15px !important;
          }

          .small-box .icon {
            font-size: 58px !important;
          }
        }

        @media (max-width: 767px) {
          .small-box,
          .small-box > .inner {
            min-height: 155px !important;
          }

          .small-box h3 {
            font-size: 25px !important;
          }

          #vb_issue .small-box h3,
          #vb_issue h3 {
            font-size: 20px !important;
          }
        }
      ")),

      tags$style(HTML("
        /* PREIS_HEADER_CENTER_FINAL */

        .skin-blue .main-header .navbar {
          position: relative !important;
        }

        .skin-blue .main-header .navbar .preis-permanent-title {
          position: absolute !important;
          top: 0 !important;
          left: 50% !important;
          right: auto !important;
          float: none !important;
          transform: translateX(-50%) !important;

          width: calc(100% - 180px) !important;
          max-width: 1050px !important;
          height: 64px !important;

          margin: 0 !important;
          padding: 0 20px !important;

          display: flex !important;
          align-items: center !important;
          justify-content: center !important;

          text-align: center !important;
          box-sizing: border-box !important;
          z-index: 5 !important;
          pointer-events: none !important;
        }

        .preis-permanent-title-content {
          width: 100% !important;
          height: 64px !important;

          display: flex !important;
          flex-direction: column !important;
          align-items: center !important;
          justify-content: center !important;

          text-align: center !important;
          line-height: 1.12 !important;
        }

        .preis-permanent-main-title {
          width: 100% !important;
          margin: 0 !important;

          color: white !important;
          font-size: 21px !important;
          font-weight: 800 !important;
          text-align: center !important;
          white-space: nowrap !important;
        }

        .preis-permanent-subtitle {
          width: 100% !important;
          margin: 5px 0 0 0 !important;

          color: white !important;
          font-size: 13px !important;
          font-weight: 400 !important;
          text-align: center !important;
          white-space: nowrap !important;
          opacity: 0.96 !important;
        }

        @media (max-width: 1100px) {
          .skin-blue .main-header .navbar .preis-permanent-title {
            width: calc(100% - 110px) !important;
            padding: 0 10px !important;
          }

          .preis-permanent-main-title {
            font-size: 17px !important;
          }

          .preis-permanent-subtitle {
            font-size: 11px !important;
          }
        }

        @media (max-width: 760px) {
          .preis-permanent-subtitle {
            display: none !important;
          }

          .preis-permanent-main-title {
            font-size: 14px !important;
            white-space: normal !important;
          }
        }
      ")),
      tags$style(HTML("
        .skin-blue .main-header .logo {background: linear-gradient(90deg, #1E7F67 0%, #7A1D2F 100%) !important; color:white !important; font-weight:700;}
        .skin-blue .main-header .navbar {background: linear-gradient(90deg, #1E7F67 0%, #7A1D2F 100%) !important;}
        .skin-blue .main-header .logo {height:64px !important; line-height:64px !important; padding:5px 10px !important;}
        .skin-blue .main-header .navbar {min-height:64px !important;}
        .skin-blue .main-sidebar {padding-top:64px !important;}
        .content-wrapper {padding-top:14px !important;}
        .africacdc-header-logo {width:125px; max-width:48%; height:auto; vertical-align:middle; margin-right:8px;}
        .preis-header-title {font-size:17px; font-weight:800; vertical-align:middle; white-space:nowrap;}
        .preis-permanent-title {
          float:left !important;
          height:64px !important;
          margin:0 !important;
          padding:0 15px !important;
          list-style:none !important;
          pointer-events:none;
        }
        .preis-permanent-title-content {
          height:64px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          color:white;
          line-height:1.1;
        }
        .preis-permanent-main-title {
          font-size:20px;
          font-weight:800;
          white-space:nowrap;
        }
        .preis-permanent-subtitle {
          margin-top:4px;
          font-size:12px;
          font-weight:400;
          opacity:0.95;
          white-space:nowrap;
        }
        @media (max-width:1100px) {
          .preis-permanent-main-title {font-size:16px;}
          .preis-permanent-subtitle {font-size:10px;}
        }
        @media (max-width:850px) {
          .preis-permanent-subtitle {display:none;}
          .preis-permanent-main-title {
            font-size:14px;
            white-space:normal;
          }
        }
        .skin-blue .main-sidebar {background-color:#0f2f36 !important;}
        .content-wrapper, .right-side {background-color:#f2f4f7 !important;}
        body, .form-control, .selectize-input, .selectize-dropdown, .control-label {font-size:17px !important;}
        .sidebar-menu>li>a {font-size:18px !important; padding-top:15px !important; padding-bottom:15px !important;}
        .sidebar-menu>li>a>.fa, .sidebar-menu>li>a>.fas, .sidebar-menu>li>a>.far {font-size:21px !important; width:25px !important;}
        .btn {font-size:16px !important; font-weight:700 !important;}
        .small-box {border-radius:12px !important; box-shadow:0 2px 8px rgba(0,0,0,0.08) !important; min-height:136px !important;}
        .small-box .inner {min-height:100px !important;}
        .small-box h3 {font-size:42px !important; font-weight:800 !important;}
        .small-box p {font-size:18px !important; min-height:42px !important;}
        .box {border-radius:12px; box-shadow:0 2px 8px rgba(0,0,0,0.08) !important;}
        .box-title {font-size:20px !important; font-weight:700 !important;}
        .box.box-solid.box-primary>.box-header {background:#1E7F67 !important;}
        .note-block {font-size:18px; line-height:1.6; color:#2d3436;}
        .leaflet-control, .leaflet-popup-content {font-size:16px !important;}
        .leaflet-control-layers, .leaflet-control-attribution {font-size:14px !important;}

        /* Stationary Africa CDC alert pulse: no translation or scale movement. */
        .polio-blink {
          animation: polioPulse 2.4s ease-in-out infinite;
          transform-origin: center;
          transform-box: fill-box;
        }
        @keyframes polioPulse {
          0%, 38% {opacity:1; fill-opacity:.98; stroke-opacity:1; filter:drop-shadow(0 0 7px rgba(242,142,43,.78));}
          50%, 66% {opacity:0; fill-opacity:0; stroke-opacity:0; filter:none;}
          78%, 100% {opacity:1; fill-opacity:.98; stroke-opacity:1; filter:drop-shadow(0 0 7px rgba(30,127,103,.78));}
        }

        /* The dedicated map tab uses the full available content viewport. */
        #shiny-tab-map {padding:0 !important; margin:-14px -15px -15px -15px !important; height:calc(100vh - 64px) !important; overflow:hidden !important;}
        #shiny-tab-map > .row {margin:0 !important;}
        #shiny-tab-map .col-sm-12 {padding:0 !important;}
        #map_full {height:calc(100vh - 64px) !important; min-height:720px !important; width:100% !important;}
        .leaflet-container {background:#eef3f1;}
      "))
    ),
    tabItems(
      tabItem(
        tabName = "overview",

        fluidRow(
          valueBoxOutput("vb_cases", 2),
          valueBoxOutput("vb_countries", 2),
          valueBoxOutput("vb_rcc", 2),
          valueBoxOutput("vb_issue", 2),
          valueBoxOutput("vb_human", 2),
          valueBoxOutput("vb_env", 2)
        ),
        fluidRow(
          box(
            width = 8, title = "Polio Africa map", status = "primary", solidHeader = TRUE,
            leafletOutput("map_overview", height = 560)
          ),
          box(
            width = 4, title = "RCC and epidemiological profile", status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_rcc", height = 200),
            plotlyOutput("plot_virus", height = 180),
            plotlyOutput("plot_detect", height = 180)
          )
        ),
        fluidRow(
          box(
            width = 7, title = "Temporal evolution of detections", status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_time", height = 280)
          ),
          box(
            width = 5, title = "Operational interpretation", status = "primary", solidHeader = TRUE,
            div(
              class = "note-block",
              p("This dashboard is an unofficial prototype. It displays polio signals published by GPEI (Polio This Week). The RCC grouping is used only as a geographic classification scheme; this is not an official product of Africa CDC, WHO or GPEI."),
              p("Alerts are prepared for RCCs; sending is currently limited to the author."),
              p("Affected Member States are shown with polio markers."),
              p("The dashboard distinguishes detections in humans and environmental surveillance."),
              p("The same filters drive the map, charts, RCC summaries, country details, and downloadable data.")
            )
          )
        ),
        fluidRow(
          box(width = 6, title = "RCC summary", status = "primary", solidHeader = TRUE, DTOutput("tbl_rcc_summary")),
          box(width = 6, title = "Country details", status = "primary", solidHeader = TRUE, DTOutput("tbl_country_summary"))
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            width = 12, status = "warning", solidHeader = TRUE,
            title = "Interpretation rule",
            div(
              class = "note-block",
              p("These are detections reported by GPEI publication date, not incidence by paralysis onset or specimen collection date."),
              p("Missing publications are treated as unavailable data, never as zero. Historical coverage varies by year.")
            )
          )
        ),
        fluidRow(
          box(width = 8, title = "Detections reported by GPEI publication date", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_history_time", height = 360)),
          box(width = 4, title = "Archive coverage by year", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_coverage", height = 360))
        ),
        fluidRow(
          box(width = 6, title = "Reported detections by virus type", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_history_virus", height = 320)),
          box(width = 6, title = "Member States with most reported detections", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_top_countries", height = 320))
        ),
        fluidRow(
          box(width = 12, title = "Historical data quality and coverage", status = "primary", solidHeader = TRUE,
              DTOutput("tbl_history_quality"))
        )
      ),
      tabItem(
        tabName = "map",
        fluidRow(
          column(width = 12, leafletOutput("map_full", height = "calc(100vh - 64px)"))
        )
      ),
      tabItem(
        tabName = "records",
        fluidRow(
          box(width = 12, title = "Filtered polio records", status = "primary", solidHeader = TRUE, DTOutput("tbl_polio"))
        )
      ),
      tabItem(
        tabName = "downloads",
        fluidRow(
          box(width = 6, title = "Download filtered data", status = "primary", solidHeader = TRUE, downloadButton("dl_polio", "Polio records (filtered)")),
          box(
            width = 6, title = "Note", status = "primary", solidHeader = TRUE,
            div(
              class = "note-block",
              p("This app uses the real PREIS_Polio_FV input files."),
              p("It is aligned with the project RCC mapping and Africa RCC geojson."),
              p("The exported file includes virus type and detection source.")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  polio_df_reactive <- reactive({
    invalidateLater(AUTO_REFRESH_MS, session)
    x <- build_polio_df()
    message("[APP] auto-refresh polio data -> rows: ", nrow(x), " | time: ", Sys.time())
    x
  })

  # ---- Keep the "Latest" view on the newest edition after an auto-refresh ----
  # Bug fixed 26 Nov 2026: date_max was computed once at start-up, so when a new GPEI
  # edition arrived during an open session the date filter stayed on the old date and
  # every card showed 0 ("No dated issue"). Now the filter follows the newest edition.
  latest_seen <- reactiveVal(date_max)
  observeEvent(polio_df_reactive(), {
    d <- polio_df_reactive()$issue_date
    d <- d[!is.na(d)]
    if (!length(d)) return()
    new_max <- max(d)
    old_max <- latest_seen()
    if (identical(new_max, old_max)) return()
    dr <- input$date_range
    latest_seen(new_max)
    if (!is.null(dr) && length(dr) == 2 && !anyNA(dr)) {
      if (dr[1] == old_max && dr[2] == old_max) {
        # user was on "Latest": move to the new latest edition
        updateDateRangeInput(session, "date_range", start = new_max, end = new_max)
        ny <- as.integer(format(new_max, "%Y"))
        updateSliderInput(session, "year_range", value = c(ny, ny))
      } else if (dr[2] == old_max) {
        # user was on "Full history": extend the end date
        updateDateRangeInput(session, "date_range", end = new_max)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$reset_filters, {
    updateSelectInput(session, "rcc", selected = "All")
    updateSelectInput(session, "country", selected = "All")
    updateSelectInput(session, "virus_type", selected = "All")
    updateSelectInput(session, "detection_source", selected = "All")
    ly <- as.integer(format(latest_seen(), "%Y"))
    updateSliderInput(session, "year_range", value = c(ly, ly))
    updateDateRangeInput(session, "date_range", start = latest_seen(), end = latest_seen())
  })

  observeEvent(input$show_history, {
    updateSliderInput(session, "year_range", value = c(min(year_levels), max(year_levels)))
    updateDateRangeInput(session, "date_range", start = date_min, end = latest_seen())
  })
  
  observe({
    df <- polio_df_reactive()
    
    if (!is.null(input$rcc) && input$rcc != "All") {
      df <- df %>% filter(rcc == input$rcc)
    }
    if (!is.null(input$virus_type) && input$virus_type != "All") {
      df <- df %>% filter(virus_type == input$virus_type)
    }
    if (!is.null(input$detection_source) && input$detection_source != "All") {
      df <- df %>% filter(detection_source == input$detection_source)
    }
    if (!is.null(input$date_range) && length(input$date_range) == 2) {
      df <- df %>%
        filter(is.na(issue_date) | (issue_date >= input$date_range[1] & issue_date <= input$date_range[2]))
    }
    if (!is.null(input$year_range) && length(input$year_range) == 2) {
      df <- df %>%
        filter(
          is.na(issue_date) |
            (as.integer(format(issue_date, "%Y")) >= input$year_range[1] &
               as.integer(format(issue_date, "%Y")) <= input$year_range[2])
        )
    }
    
    countries_now <- sort(unique(df$country))
    countries_now <- countries_now[countries_now != "" & !is.na(countries_now)]
    
    selected_country <- "All"
    if (!is.null(input$country) && input$country %in% c("All", countries_now)) {
      selected_country <- input$country
    }
    
    updateSelectInput(
      session,
      "country",
      choices = c("All", countries_now),
      selected = selected_country
    )
  })
  
  filt_polio <- reactive({
    x <- polio_df_reactive()
    
    if (!is.null(input$rcc) && input$rcc != "All") {
      x <- x %>% filter(rcc == input$rcc)
    }
    if (!is.null(input$country) && input$country != "All") {
      x <- x %>% filter(country == input$country)
    }
    if (!is.null(input$virus_type) && input$virus_type != "All") {
      x <- x %>% filter(virus_type == input$virus_type)
    }
    if (!is.null(input$detection_source) && input$detection_source != "All") {
      x <- x %>% filter(detection_source == input$detection_source)
    }
    if (!is.null(input$date_range) && length(input$date_range) == 2) {
      x <- x %>%
        filter(is.na(issue_date) | (issue_date >= input$date_range[1] & issue_date <= input$date_range[2]))
    }
    if (!is.null(input$year_range) && length(input$year_range) == 2) {
      x <- x %>%
        filter(
          is.na(issue_date) |
            (as.integer(format(issue_date, "%Y")) >= input$year_range[1] &
               as.integer(format(issue_date, "%Y")) <= input$year_range[2])
        )
    }
    
    x
  })
  
  # RCC background must stay fixed regardless of filters
  # RCC background must stay fixed regardless of filters
  map_polygons <- reactive({
    x <- filt_polio() %>%
      mutate(
        country = canon_country_name(country),
        iso3    = toupper(norm_txt(iso3)),
        rcc_geo = clean_rcc(rcc),
        rcc_ov  = .COUNTRY_RCC_OVERRIDE[country],
        rcc     = dplyr::coalesce(
          as.character(rcc_ov),
          dplyr::na_if(as.character(rcc_geo), "Unspecified"),
          "Unspecified"
        )
      ) %>%
      select(-rcc_geo, -rcc_ov)
    
    # Primary aggregation by country
    agg_country <- x %>%
      group_by(country) %>%
      summarise(
        total_cases_country = sum(cases, na.rm = TRUE),
        n_verify_country = sum(is.na(cases)),
        n_records_country = n(),
        virus_types_country = paste(sort(unique(virus_type[virus_type != ""])), collapse = ", "),
        detection_sources_country = paste(sort(unique(detection_source[detection_source != ""])), collapse = ", "),
        .groups = "drop"
      )
    
    # Secondary aggregation by iso3
    agg_iso3 <- x %>%
      filter(!is.na(iso3), iso3 != "") %>%
      group_by(iso3) %>%
      summarise(
        total_cases_iso3 = sum(cases, na.rm = TRUE),
        n_verify_iso3 = sum(is.na(cases)),
        n_records_iso3 = n(),
        virus_types_iso3 = paste(sort(unique(virus_type[virus_type != ""])), collapse = ", "),
        detection_sources_iso3 = paste(sort(unique(detection_source[detection_source != ""])), collapse = ", "),
        .groups = "drop"
      )
    
    shp <- africa_map %>%
      mutate(
        country  = canon_country_name(country),
        iso3     = toupper(norm_txt(iso3)),
        rcc_geo  = clean_rcc(rcc),
        rcc_ov   = .COUNTRY_RCC_OVERRIDE[country],
        rcc      = dplyr::coalesce(rcc_ov, ifelse(rcc_geo=="Unspecified", NA_character_, rcc_geo), "Unspecified")
      ) %>%
      select(-rcc_geo, -rcc_ov) %>%
      left_join(agg_country, by = "country") %>%
      left_join(agg_iso3, by = "iso3") %>%
      mutate(
        total_cases = dplyr::coalesce(total_cases_country, total_cases_iso3, 0),
        n_verify = dplyr::coalesce(n_verify_country, n_verify_iso3, 0L),
        n_records = dplyr::coalesce(n_records_country, n_records_iso3, 0),
        virus_types = dplyr::coalesce(virus_types_country, virus_types_iso3, ""),
        detection_sources = dplyr::coalesce(detection_sources_country, detection_sources_iso3, "")
      ) %>%
      select(
        -total_cases_country, -n_records_country, -virus_types_country, -detection_sources_country,
        -total_cases_iso3, -n_records_iso3, -virus_types_iso3, -detection_sources_iso3,
        -n_verify_country, -n_verify_iso3
      )
    
    shp
  })
  
  map_markers <- reactive({
    x <- filt_polio() %>%
      mutate(
        country = canon_country_name(country),
        iso3 = toupper(norm_txt(iso3)),
        rcc = clean_rcc(rcc),
        virus_type = ifelse(is.na(virus_type) | virus_type == "", "Unspecified", virus_type),
        detection_source = ifelse(is.na(detection_source) | detection_source == "", "Unspecified", detection_source),
        lon = suppressWarnings(as.numeric(lon)),
        lat = suppressWarnings(as.numeric(lat))
      ) %>%
      filter(!is.na(lon), !is.na(lat))
    
    cat("[APP] map_markers rows:", nrow(x), "\n")
    
    if (nrow(x) == 0) {
      return(x)
    }
    
    x <- x %>%
      group_by(country, iso3, rcc, virus_type, detection_source) %>%
      summarise(
        lon = dplyr::first(lon),
        lat = dplyr::first(lat),
        n_to_verify = sum(is.na(cases)),
        cases = sum(cases, na.rm = TRUE),
        n_records = dplyr::n(),
        issue_date = if (all(is.na(issue_date))) {
          as.Date(NA)
        } else {
          max(issue_date, na.rm = TRUE)
        },
        issue_id = paste(unique(issue_id[issue_id != ""]), collapse = "; "),
        signal_type = paste(unique(signal_type[signal_type != ""]), collapse = ", "),
        summary_text = paste(unique(summary_text[summary_text != ""]), collapse = " | "),
        source_url = dplyr::first(source_url[source_url != ""]),
        url_link = dplyr::first(url_link[url_link != ""]),
        .groups = "drop"
      ) %>%
      mutate(
        icon = mapply(
          FUN = polio_detection_icon,
          virus_type,
          detection_source,
          USE.NAMES = FALSE
        )
      )
    
    spread_country_markers(x)
  })
  
  render_polio_map <- function() {
    shp <- map_polygons()
    mks <- map_markers()
    
    # Palette avec levels fixes pour garantir la correspondance couleur-RCC
    rcc_levels_fixed <- c("Northern","Western","Central","Eastern","Southern")
    rcc_colors_fixed <- c("#E3A008","#0F9D58","#8E44AD","#0072B2","#B3261E")
    pal <- colorFactor(
      palette  = rcc_colors_fixed,
      levels   = rcc_levels_fixed,
      na.color = "#9AA0A6"
    )
    
    m <- leaflet(
      options = leafletOptions(
        minZoom = 3,
        maxZoom = 6,
        zoomControl = TRUE,
        dragging = TRUE
      )
    ) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      fitBounds(lng1 = -25, lat1 = -38, lng2 = 60, lat2 = 40) %>%
      setMaxBounds(lng1 = -30, lat1 = -40, lng2 = 65, lat2 = 42)
    
    if (isTRUE(input$show_rcc_layer) && nrow(shp) > 0) {
      m <- m %>%
        addPolygons(
          data = shp,
          fillColor = ~pal(rcc),
          fillOpacity = 0.72,
          color = "#34495E",
          weight = 1,
          smoothFactor = 0.2,
          popup = ~paste0(
            "<b>", country, "</b><br/>",
            "<b>RCC:</b> ", rcc, "<br/>",
            "<b>Detections (human + environmental):</b> ",
            ifelse(n_verify > 0 & total_cases == 0, "count to verify",
                   paste0(total_cases, ifelse(n_verify > 0, paste0(" (+ ", n_verify, " record(s) to verify)"), ""))),
            "<br/>",
            "<b>Records:</b> ", n_records, "<br/>",
            "<b>Virus type(s):</b> ", ifelse(virus_types == "", "None in filtered data", virus_types), "<br/>",
            "<b>Detection source(s):</b> ", ifelse(detection_sources == "", "None in filtered data", detection_sources)
          ),
          group = "RCC layer"
        ) %>%
        addLegend(
          position = "bottomleft",
          colors = rcc_colors_fixed,
          labels = rcc_levels_fixed,
          title = "Africa CDC RCC",
          opacity = 0.9
        )
    }
    
    if (isTRUE(input$show_case_markers) && nrow(mks) > 0) {
      mks <- mks %>%
        mutate(
          marker_label = icon,
          marker_fill = virus_colour(virus_type),
          marker_stroke = marker_fill,
          marker_radius = pmin(34, pmax(13, sqrt(pmax(cases, 1)) * 2.35)),
          cases_label = dplyr::case_when(
            n_to_verify > 0 & cases == 0 ~ "count to verify",
            n_to_verify > 0 ~ paste0(cases, " (+ ", n_to_verify, " record(s) to verify)"),
            TRUE ~ as.character(cases)
          )
        )
      
      m <- m %>%
        addCircleMarkers(
          data = mks,
          lng = ~lon,
          lat = ~lat,
          radius = ~marker_radius,
          stroke = TRUE,
          weight = 3,
          color = ~marker_stroke,
          fillColor = ~marker_fill,
          fillOpacity = 0.95,
          popup = ~paste0(
            "<b>", htmlEscape(country), "</b><br/>",
            "<b>RCC:</b> ", htmlEscape(rcc), "<br/>",
            "<b>Virus type:</b> ", htmlEscape(virus_type), "<br/>",
            "<b>Detection source:</b> ", htmlEscape(detection_source), "<br/>",
            "<b>Detections:</b> ", cases_label, "<br/>",
            "<b>Records:</b> ", n_records, "<br/>",
            "<b>Latest issue date:</b> ", ifelse(is.na(issue_date), "", as.character(issue_date)), "<br/>",
            ifelse(issue_id == "", "", paste0("<b>Issue ID:</b> ", htmlEscape(issue_id), "<br/>")),
            ifelse(signal_type == "", "", paste0("<b>Signal type:</b> ", htmlEscape(signal_type), "<br/>")),
            ifelse(summary_text == "", "", paste0("<b>Summary:</b> ", htmlEscape(summary_text), "<br/>")),
            ifelse(url_link == "", "", url_link)
          ),
          options = pathOptions(className = "polio-blink"),
          group = "Polio markers"
        ) %>%
        addLabelOnlyMarkers(
          data = mks,
          lng = ~lon,
          lat = ~lat,
          label = ~marker_label,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "font-size" = "13px",
              "font-weight" = "800",
              "color" = "white",
              "background" = "transparent",
              "border" = "none",
              "box-shadow" = "none",
              "text-align" = "center",
              "line-height" = "14px",
              "text-shadow" = "0 1px 3px rgba(0,0,0,.85)"
            )
          ),
          group = "Polio markers"
        ) %>%
        addControl(
          html = HTML(
            paste0(
              "<div style='background:white;padding:12px 14px;border-radius:10px;border:2px solid #1E7F67;font-size:16px;line-height:1.55;'>",
              "<b>Marker meaning</b><br>",
              "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;background:#C62828;margin-right:6px;'></span> H = Human<br>",
              "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;background:#C62828;margin-right:6px;'></span> E = Environmental<br>",
              "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;background:#C62828;margin-right:6px;'></span> HE = Both",
              "</div>"
            )
          ),
          position = "topright"
        )
    }
    
    m %>%
      addLayersControl(
        overlayGroups = c("RCC layer", "Polio markers"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  output$vb_cases <- renderValueBox({
    n_ver <- sum(is.na(filt_polio()$cases))
    valueBox(
      comma(sum(filt_polio()$cases, na.rm = TRUE)),
      paste0("Total detections (human + environmental)",
             ifelse(n_ver > 0, paste0(" - ", n_ver, " record(s) to verify"), "")),
      icon = icon("virus"), color = "red"
    )
  })
  
  output$vb_countries <- renderValueBox({
    n <- filt_polio() %>% summarise(n = n_distinct(country[country != ""])) %>% pull(n)
    valueBox(comma(n), "Affected Member States", icon = icon("globe-africa"), color = "yellow")
  })
  
  output$vb_rcc <- renderValueBox({
    n <- filt_polio() %>% summarise(n = n_distinct(rcc[rcc != "" & rcc != "Unspecified"])) %>% pull(n)
    valueBox(comma(n), "RCC represented", icon = icon("layer-group"), color = "aqua")
  })
  
  output$vb_issue <- renderValueBox({
    d <- filt_polio()$issue_date
    d <- d[!is.na(d)]
    lab <- if (length(d) == 0) {
      "No dated issue"
    } else {
      format(max(d), "%d %b %Y")
    }
    valueBox(lab, "Latest issue date", icon = icon("calendar"), color = "green")
  })
  
  output$vb_human <- renderValueBox({
    n <- filt_polio() %>%
      filter(detection_source %in% c("Human", "Both")) %>%
      summarise(v = sum(cases, na.rm = TRUE)) %>%
      pull(v)
    valueBox(comma(n), "Human detections", icon = icon("user"), color = "light-blue")
  })
  
  output$vb_env <- renderValueBox({
    n <- filt_polio() %>%
      filter(detection_source %in% c("Environmental", "Both")) %>%
      summarise(v = sum(cases, na.rm = TRUE)) %>%
      pull(v)
    valueBox(comma(n), "Environmental detections", icon = icon("water"), color = "olive")
  })
  
  output$plot_rcc <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(rcc), rcc != "") %>%
      count(rcc, wt = cases, name = "cases", sort = TRUE)
    
    if (nrow(df) == 0) return(empty_plotly("No RCC data available."))
    
    p <- ggplot(df, aes(
      x = reorder(rcc, cases),
      y = cases,
      fill = rcc,
      text = paste0("RCC: ", rcc, "<br>Detections: ", cases)
    )) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(y = cases + label_nudge(cases), label = comma(cases)), color = "#173F3A", fontface = "bold", size = 3.5) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_fill_manual(values = rcc_palette, drop = FALSE) +
      labs(x = NULL, y = "Detections") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_virus <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(virus_type), virus_type != "") %>%
      count(virus_type, wt = cases, name = "cases", sort = TRUE)
    
    if (nrow(df) == 0) return(empty_plotly("No virus type data available."))
    
    p <- ggplot(df, aes(
      x = reorder(virus_type, cases),
      y = cases,
      fill = virus_type,
      text = paste0("Virus type: ", virus_type, "<br>Detections: ", cases)
    )) +
      geom_col() +
      geom_text(aes(y = cases + label_nudge(cases), label = comma(cases)), color = "#173F3A", fontface = "bold", size = 3.5) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_fill_manual(values = virus_palette, drop = FALSE) +
      labs(x = NULL, y = "Detections") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_detect <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(detection_source), detection_source != "") %>%
      count(detection_source, wt = cases, name = "cases", sort = TRUE)
    
    if (nrow(df) == 0) return(empty_plotly("No detection source data available."))
    
    p <- ggplot(df, aes(
      x = reorder(detection_source, cases),
      y = cases,
      fill = detection_source,
      text = paste0("Detection source: ", detection_source, "<br>Detections: ", cases)
    )) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(y = cases + label_nudge(cases), label = comma(cases)), color = "#173F3A", fontface = "bold", size = 3.5) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_fill_manual(values = detect_palette, drop = FALSE) +
      labs(x = NULL, y = "Detections") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5.5, 30, 5.5, 5.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_time <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(issue_date)) %>%
      group_by(issue_date, virus_type) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) return(empty_plotly("No dated time series available."))
    
    p <- ggplot(df, aes(
      x = issue_date,
      y = cases,
      color = virus_type,
      text = paste0("Date: ", issue_date, "<br>Virus type: ", virus_type, "<br>Detections: ", cases)
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_text(aes(y = cases + label_nudge(cases, 0.06), label = ifelse(cases > 0, comma(cases), "")), size = 3, show.legend = FALSE) +
      scale_color_manual(values = virus_palette, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
      labs(x = NULL, y = "Detections", color = "Virus type") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })

  output$plot_history_time <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(issue_date)) %>%
      group_by(issue_date, virus_type) %>%
      summarise(detections = sum(cases, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return(empty_plotly("No historical publication data for these filters."))
    p <- ggplot(df, aes(
      issue_date, detections, colour = virus_type,
      text = paste0(
        "Publication date: ", issue_date,
        "<br>Virus type: ", virus_type,
        "<br>Reported detections: ", comma(detections)
      )
    )) +
      geom_line(aes(group = virus_type), linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 1.8) +
      geom_text(aes(y = detections + label_nudge(detections, 0.06), label = ifelse(detections > 0, comma(detections), "")), size = 2.8, show.legend = FALSE) +
      scale_color_manual(values = virus_palette, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
      labs(x = NULL, y = "Reported detections", colour = "Virus type") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$plot_coverage <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(issue_date)) %>%
      mutate(year = as.integer(format(issue_date, "%Y"))) %>%
      group_by(year) %>%
      summarise(publications_available = n_distinct(issue_date), .groups = "drop")
    if (!nrow(df)) return(empty_plotly("No coverage data for these filters."))
    p <- ggplot(df, aes(
      factor(year), publications_available,
      text = paste0("Year: ", year, "<br>Available publication dates: ", publications_available)
    )) +
      geom_col(fill = "#1E7F67") +
      geom_text(aes(y = publications_available + label_nudge(publications_available, 0.06), label = publications_available), color = "#173F3A", fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Publication year", y = "Available publication dates") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$plot_history_virus <- renderPlotly({
    df <- filt_polio() %>%
      filter(virus_type != "", virus_type != "Unspecified") %>%
      group_by(virus_type) %>%
      summarise(detections = sum(cases, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return(empty_plotly("No virus data for these filters."))
    p <- ggplot(df, aes(
      virus_type, detections, fill = virus_type,
      text = paste0(
        "Virus: ", virus_type,
        "<br>Reported detections: ", comma(detections)
      )
    )) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(y = detections + label_nudge(detections, 0.06), label = comma(detections)), color = "#173F3A", fontface = "bold", size = 3.6) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
      scale_fill_manual(values = virus_palette, drop = FALSE) +
      labs(x = NULL, y = "Reported detections") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$plot_top_countries <- renderPlotly({
    df <- filt_polio() %>%
      filter(country != "") %>%
      group_by(country) %>%
      summarise(detections = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(detections)) %>%
      slice_head(n = 12)
    if (!nrow(df)) return(empty_plotly("No Member State data for these filters."))
    p <- ggplot(df, aes(
      reorder(country, detections), detections,
      fill = "Africa CDC",
      text = paste0("Member State: ", country, "<br>Reported detections: ", comma(detections))
    )) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(y = detections + label_nudge(detections), label = comma(detections)), color = "#173F3A", fontface = "bold", size = 3.5) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_fill_manual(values = c("Africa CDC" = "#1E7F67")) +
      labs(x = NULL, y = "Reported detections") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5.5, 34, 5.5, 5.5))
    ggplotly(p, tooltip = "text")
  })

  output$tbl_history_quality <- renderDT({
    df <- filt_polio() %>%
      filter(!is.na(issue_date)) %>%
      mutate(year = as.integer(format(issue_date, "%Y"))) %>%
      group_by(year) %>%
      summarise(
        first_publication = min(issue_date),
        last_publication = max(issue_date),
        publication_dates_available = n_distinct(issue_date),
        Member_States = n_distinct(country[country != ""]),
        records = n(),
        interpretation = "Available GPEI publications; missing dates are not zero",
        .groups = "drop"
      ) %>%
      arrange(year)
    datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$map_overview <- renderLeaflet({
    render_polio_map()
  })
  
  output$map_full <- renderLeaflet({
    render_polio_map()
  })
  
  output$tbl_rcc_summary <- renderDT({
    df <- filt_polio() %>%
      group_by(rcc) %>%
      summarise(
        countries = n_distinct(country[country != ""]),
        total_detections = sum(cases, na.rm = TRUE),
        human_cases = sum(cases[detection_source %in% c("Human", "Both")], na.rm = TRUE),
        environmental_detections = sum(cases[detection_source %in% c("Environmental", "Both")], na.rm = TRUE),
        records_to_verify = sum(is.na(cases)),
        .groups = "drop"
      ) %>%
      arrange(desc(total_detections), rcc)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  output$tbl_country_summary <- renderDT({
    df <- filt_polio() %>%
      group_by(country, rcc, virus_type, detection_source) %>%
      summarise(
        total_detections = sum(cases, na.rm = TRUE),
        records_to_verify = sum(is.na(cases)),
        latest_issue = if (all(is.na(issue_date))) {
          as.Date(NA)
        } else {
          max(issue_date, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      arrange(desc(latest_issue), rcc, country)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  output$tbl_polio <- renderDT({
    df <- filt_polio() %>%
      arrange(desc(issue_date), rcc, country) %>%
      transmute(
        issue_date,
        issue_id,
        rcc,
        country,
        iso3,
        virus_type,
        detection_source,
        signal_type,
        cases,
        summary_text,
        link = url_link
      )
    
    datatable(
      df,
      escape = FALSE,
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  output$dl_polio <- downloadHandler(
    filename = function() {
      paste0("preis_polio_fv_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filt_polio(), file, na = "")
    }
  )
}

shinyApp(ui, server)

