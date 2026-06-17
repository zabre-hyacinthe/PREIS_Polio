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

POLIO_EMAIL_FP <- file.path(DASH_DATA_DIR, "polio_africa_email_input.csv")
POLIO_ALERT_FP <- file.path(DASH_DATA_DIR, "polio_alert_input.csv")
POLIO_ISSUE_FP <- file.path(DASH_DATA_DIR, "polio_last_issue.csv")
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
  if (!file.exists(path)) return(tibble())
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("[READ ERROR] ", path, " :: ", e$message)
      tibble()
    }
  )
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
    str_detect(z, "CVDPV") ~ "cVDPV",
    str_detect(z, "WPV1") ~ "WPV1",
    str_detect(z, "WPV") ~ "WPV",
    z == "" ~ "Unspecified",
    TRUE ~ z
  )
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
  polio_email <- safe_read_csv(POLIO_EMAIL_FP)
  polio_alert <- safe_read_csv(POLIO_ALERT_FP)
  polio_issue <- safe_read_csv(POLIO_ISSUE_FP)
  
  names(polio_email) <- clean_colnames(names(polio_email))
  names(polio_alert) <- clean_colnames(names(polio_alert))
  names(polio_issue) <- clean_colnames(names(polio_issue))
  
  if (nrow(polio_email) > 0) {
    polio_raw <- polio_email
  } else {
    polio_raw <- polio_alert
  }
  
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
    "virus_type", "signal_type", "count", "summary_text", "source_url",
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
      detection_source = clean_detection_source(signal_type),
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
      cases = ifelse(is.na(cases) | cases <= 0, 1, cases),
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
    )
  
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

if (file.exists(LOGO_FP)) {
  header_title <- tags$span(
    tags$img(src = "africacdc_logo.png", height = "30px", style = "margin-right:10px; vertical-align:middle;"),
    tags$span("PREIS-POLIO", style = "vertical-align:middle;")
  )
} else {
  header_title <- "PREIS-POLIO"
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = header_title),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      selected = "overview",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("globe-africa")),
      menuItem("Records", tabName = "records", icon = icon("table")),
      menuItem("Downloads", tabName = "downloads", icon = icon("download"))
    ),
    br(),
    selectInput("rcc", "RCC", choices = c("All", rcc_levels), selected = "All"),
    selectInput("country", "Member State", choices = c("All", country_levels), selected = "All"),
    selectInput("virus_type", "Virus type", choices = c("All", virus_levels), selected = "All"),
    selectInput("detection_source", "Detection source", choices = c("All", detect_levels), selected = "All"),
    dateRangeInput("date_range", "Issue date", start = date_min, end = date_max),
    checkboxInput("show_rcc_layer", "Show RCC layer", value = TRUE),
    checkboxInput("show_case_markers", "Show polio markers", value = TRUE)
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {background: linear-gradient(90deg, #1E7F67 0%, #7A1D2F 100%) !important; color:white !important; font-weight:700;}
        .skin-blue .main-header .navbar {background: linear-gradient(90deg, #1E7F67 0%, #7A1D2F 100%) !important;}
        .skin-blue .main-sidebar {background-color:#0f2f36 !important;}
        .content-wrapper, .right-side {background-color:#f2f4f7 !important;}
        .small-box {border-radius:12px !important; box-shadow:0 2px 8px rgba(0,0,0,0.08) !important; min-height:136px !important;}
        .small-box .inner {min-height:100px !important;}
        .small-box h3 {font-size:34px !important; font-weight:700 !important;}
        .small-box p {font-size:15px !important; min-height:38px !important;}
        .box {border-radius:12px; box-shadow:0 2px 8px rgba(0,0,0,0.08) !important;}
        .box.box-solid.box-primary>.box-header {background:#1E7F67 !important;}
        .note-block {font-size:15px; line-height:1.55; color:#2d3436;}

        .polio-blink {
          animation: polioBlink 1s infinite alternate;
        }

        @keyframes polioBlink {
          from {
            transform: scale(1.0);
            opacity: 0.95;
          }
          to {
            transform: scale(1.18);
            opacity: 1;
          }
        }
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
              p("This dashboard integrates Africa CDC RCC geographic intelligence with polio surveillance data."),
              p("RCC polygons are coloured using the Africa CDC regional palette."),
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
        tabName = "map",
        fluidRow(
          box(
            width = 12, title = "Interactive RCC map with synchronized filters", status = "primary", solidHeader = TRUE,
            leafletOutput("map_full", height = 760)
          )
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
        rcc     = dplyr::coalesce(rcc_ov, ifelse(rcc_geo=="Unspecified", NA_character_, rcc_geo), "Unspecified")
      ) %>%
      select(-rcc_geo, -rcc_ov)
    
    # Primary aggregation by country
    agg_country <- x %>%
      group_by(country) %>%
      summarise(
        total_cases_country = sum(cases, na.rm = TRUE),
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
        n_records = dplyr::coalesce(n_records_country, n_records_iso3, 0),
        virus_types = dplyr::coalesce(virus_types_country, virus_types_iso3, ""),
        detection_sources = dplyr::coalesce(detection_sources_country, detection_sources_iso3, "")
      ) %>%
      select(
        -total_cases_country, -n_records_country, -virus_types_country, -detection_sources_country,
        -total_cases_iso3, -n_records_iso3, -virus_types_iso3, -detection_sources_iso3
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
      addProviderTiles("CartoDB.Positron") %>%
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
            "<b>Total cases:</b> ", total_cases, "<br/>",
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
          marker_fill = "#C62828",
          marker_radius = pmax(7, sqrt(pmax(cases, 1)) * 1.6)
        )
      
      m <- m %>%
        addCircleMarkers(
          data = mks,
          lng = ~lon,
          lat = ~lat,
          radius = ~marker_radius,
          stroke = TRUE,
          weight = 1.5,
          color = "#7F0000",
          fillColor = ~marker_fill,
          fillOpacity = 0.95,
          popup = ~paste0(
            "<b>", htmlEscape(country), "</b><br/>",
            "<b>RCC:</b> ", htmlEscape(rcc), "<br/>",
            "<b>Virus type:</b> ", htmlEscape(virus_type), "<br/>",
            "<b>Detection source:</b> ", htmlEscape(detection_source), "<br/>",
            "<b>Total cases:</b> ", cases, "<br/>",
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
              "font-size" = "9px",
              "font-weight" = "700",
              "color" = "white",
              "background" = "transparent",
              "border" = "none",
              "box-shadow" = "none",
              "text-align" = "center",
              "line-height" = "10px"
            )
          ),
          group = "Polio markers"
        ) %>%
        addControl(
          html = HTML(
            paste0(
              "<div style='background:white;padding:8px 10px;border-radius:8px;border:1px solid #ccc;font-size:12px;'>",
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
    valueBox(comma(sum(filt_polio()$cases, na.rm = TRUE)), "Total polio cases/detections", icon = icon("virus"), color = "red")
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
      text = paste0("RCC: ", rcc, "<br>Cases: ", cases)
    )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = rcc_palette, drop = FALSE) +
      labs(x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
    
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
      text = paste0("Virus type: ", virus_type, "<br>Cases: ", cases)
    )) +
      geom_col(fill = "#7A1D2F") +
      coord_flip() +
      labs(x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
    
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
      text = paste0("Detection source: ", detection_source, "<br>Cases: ", cases)
    )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = detect_palette, drop = FALSE) +
      labs(x = NULL, y = "Cases") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_time <- renderPlotly({
    df <- filt_polio() %>%
      filter(!is.na(issue_date)) %>%
      group_by(issue_date, detection_source) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) return(empty_plotly("No dated time series available."))
    
    p <- ggplot(df, aes(
      x = issue_date,
      y = cases,
      color = detection_source,
      text = paste0("Date: ", issue_date, "<br>Detection source: ", detection_source, "<br>Cases: ", cases)
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = detect_palette, drop = FALSE) +
      labs(x = NULL, y = "Cases", color = "Detection source") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
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
        total_cases = sum(cases, na.rm = TRUE),
        human = sum(cases[detection_source %in% c("Human", "Both")], na.rm = TRUE),
        environmental = sum(cases[detection_source %in% c("Environmental", "Both")], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_cases), rcc)
    
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
        total_cases = sum(cases, na.rm = TRUE),
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
