# ============================================================
# R/lib_country_normalize.R
# PREIS POLIO — normalisation de nom de pays, source unique
#
# Utilise par PREIS_POLIO_ATOMIC_AUDIT_V2.R et
# 07_build_polio_history_dashboard.R. Ne pas dupliquer cette fonction
# ailleurs : toute correction d'alias se fait ICI, une seule fois, pour
# eviter que les scripts divergent silencieusement (cf. principe
# "toute normalisation doit etre explicite, reversible et auditable").
#
# v1.2 (repris de 07_build_polio_history_dashboard.R) :
#   base::iconv(x, from = "", to = "ASCII//TRANSLIT") depend de l'encodage
#   natif du systeme. Sur R/Windows, from = "" n'est en general PAS UTF-8,
#   alors que readr::read_csv() renvoie du texte UTF-8 : les caracteres
#   accentues et les apostrophes typographiques (Cote D'Ivoire, Dr Congo
#   (DR Congo)) passaient mal et ne se regroupaient pas correctement.
#   stringi::stri_trans_general ne depend pas du systeme.
#
# Volontairement NON resolu ici : le mot seul "Congo" (ambigu entre
# Republique Democratique du Congo et Congo Republic/Brazzaville selon le
# bulletin d'origine). Ne jamais lui assigner un pays par defaut : le
# laisser echouer la jointure et partir en revue humaine.
# ============================================================

suppressPackageStartupMessages({
  library(stringr)
})

# Verifiee par PREIS_POLIO_ATOMIC_AUDIT_V2.R au demarrage (voir note dans
# lib_atomic_parser.R).
LIB_COUNTRY_NORMALIZE_VERSION <- "1.2.0"

country_join_key <- function(x) {
  z <- as.character(x)
  z[is.na(z)] <- ""

  # Retirer les asides entre parentheses : "Democratic Republic of the
  # Congo (DR Congo)" -> "Democratic Republic of the Congo ",
  # "Central African Republic (CAR)" -> "Central African Republic ".
  z <- str_replace_all(z, "\\([^)]*\\)", " ")

  # Normaliser les apostrophes typographiques AVANT de retirer la
  # ponctuation, pour que "C\u00f4te d\u2019Ivoire" et "Cote D\u2019Ivoire"
  # donnent la meme cle quel que soit le style de guillemet ou la casse.
  z <- str_replace_all(z, "[\u2018\u2019\u02bc`]", "'")

  # Retrait des accents independant du systeme (voir note v1.2 ci-dessus).
  z <- tryCatch(
    stringi::stri_trans_general(z, "Latin-ASCII"),
    error = function(e) z
  )

  z <- tolower(z)
  z <- str_replace_all(z, "[^a-z0-9']+", " ")
  z <- str_replace_all(z, "'", "")
  z <- str_squish(z)
  z
}

#' Charge le referentiel pays ferme et construit une table de recherche
#' alias_normalise -> {country_canonical, iso2, iso3, rcc}.
#'
#' @param path Chemin vers config/country_reference_v1.csv
#' @return list(lookup = data.frame(key, country_canonical, iso2, iso3, rcc),
#'              reference = data.frame brut)
load_country_reference <- function(path) {
  if (!file.exists(path)) {
    stop("[COUNTRY REF] Reference file not found: ", path, call. = FALSE)
  }

  ref <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "")

  required_cols <- c("country_canonical", "iso2", "iso3", "rcc", "status", "aliases")
  missing_cols <- setdiff(required_cols, names(ref))
  if (length(missing_cols) > 0) {
    stop(
      "[COUNTRY REF] Missing required columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ref <- ref[is.na(ref$status) | tolower(ref$status) == "active", , drop = FALSE]

  entries <- list()
  for (i in seq_len(nrow(ref))) {
    canonical <- ref$country_canonical[i]
    iso2 <- ref$iso2[i]
    iso3 <- ref$iso3[i]
    rcc <- ref$rcc[i]

    all_names <- canonical
    if (!is.na(ref$aliases[i]) && nzchar(trimws(ref$aliases[i]))) {
      alias_list <- trimws(strsplit(ref$aliases[i], "\\|")[[1]])
      all_names <- c(all_names, alias_list)
    }

    for (nm in all_names) {
      entries[[length(entries) + 1L]] <- data.frame(
        key = country_join_key(nm),
        country_canonical = canonical,
        iso2 = iso2,
        iso3 = iso3,
        rcc = rcc,
        stringsAsFactors = FALSE
      )
    }
  }

  lookup <- do.call(rbind, entries)

  # Un doublon n'est un probleme que si la MEME cle normalisee pointe vers
  # DEUX PAYS DIFFERENTS (vrai conflit du referentiel). Si un pays a
  # simplement plusieurs alias qui se normalisent vers la meme cle que son
  # propre nom canonique (ex: "Guinea-Bissau" et "Guinea Bissau"), c'est
  # une redondance benigne, pas une alerte a lever.
  conflict_check <- stats::aggregate(
    country_canonical ~ key, data = lookup, FUN = function(x) length(unique(x))
  )
  real_conflicts <- conflict_check$key[conflict_check$country_canonical > 1]
  if (length(real_conflicts) > 0) {
    warning(
      "[COUNTRY REF] Conflicting keys mapping to DIFFERENT countries (first match wins) : ",
      paste(real_conflicts, collapse = ", "),
      call. = FALSE
    )
  }
  lookup <- lookup[!duplicated(lookup$key), , drop = FALSE]

  list(lookup = lookup, reference = ref)
}

#' Words that indicate the "country" field actually captured a narrative
#' sentence instead of a country/territory name. Used as an independent,
#' explicit gate (see PREIS_POLIO_ATOMIC_AUDIT_V2.R), in addition to (not
#' instead of) the closed-reference-only matching.
NARRATIVE_WORDS_IN_COUNTRY <- c(
  "case", "cases", "sample", "samples", "reported", "were", "was",
  "confirmed", "detected", "week", "total", "remains", "no"
)
# NB: "new" a ete volontairement retire. Il declenchait un faux positif
# sur de vrais noms de pays comme "Papua New Guinea". Les phrases
# narratives reelles documentees ("No new WPV1... were reported this
# week...") restent detectees via "no", "reported", "were", "week".

looks_like_narrative_not_country <- function(x) {
  z <- tolower(as.character(x))
  # Un seul motif combine (alternance) : grepl() n'est vectorise que sur x,
  # pas sur pattern (un vecteur de patterns silencieusement tronque au
  # premier element, avec avertissement, serait un bug ici).
  narrative_pattern <- paste0("\\b(", paste(NARRATIVE_WORDS_IN_COUNTRY, collapse = "|"), ")\\b")
  has_digit <- grepl("[0-9]", z)
  has_narrative_word <- grepl(narrative_pattern, z)
  too_long <- !is.na(z) & nchar(z) > 40
  out <- has_digit | has_narrative_word | too_long
  out[is.na(z)] <- FALSE
  out
}
