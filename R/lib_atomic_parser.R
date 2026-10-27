# ============================================================
# R/lib_atomic_parser.R
# PREIS POLIO — segmentation atomique des textes GPEI, testable seule
#
# Extrait de PREIS_POLIO_ATOMIC_AUDIT_V2.R pour permettre a
# tests/test_atomic_audit.R de charger et verifier ces fonctions sans
# dependre d'un projet complet sur disque (RAW source, referentiel pays,
# arguments CLI). Ce fichier n'a pas d'effet de bord : il ne fait que
# definir des fonctions pures.
#
# Regle : un texte peut decrire plusieurs pays. On detecte TOUTES les
# occurrences d'un motif "Mot(s) capitalise(s):" plausible comme debut de
# pays, on decoupe le texte en segments entre deux occurrences successives,
# et on extrait nombre+virus+source UNIQUEMENT a l'interieur de chaque
# segment. Aucune extraction n'est autorisee a cheval sur deux segments.
#
# Limite connue (documentee, pas cachee) : cette heuristique repose sur la
# ponctuation ":" du bulletin source (confirme present dans les donnees
# reelles observees, ex: "Democratic Republic of the Congo: four cVDPV2
# cases"). Si un futur format de bulletin change cette structure, etendre
# les tests de regression ici avant de faire confiance a une nouvelle
# annee de collecte.
#
# Depend de R/lib_country_normalize.R (looks_like_narrative_not_country) :
# le sourcer AVANT ce fichier.
# ============================================================

suppressPackageStartupMessages({
  library(stringr)
  library(tibble)
  library(dplyr)
})

# Version de ce module. Verifiee par PREIS_POLIO_ATOMIC_AUDIT_V2.R au
# demarrage : si absente ou differente de celle attendue, le script
# principal s'arrete immediatement avec un message explicite plutot que
# de tourner silencieusement avec une logique d'extraction perimee. Cette
# situation precise s'est deja produite (fichiers d'une ancienne archive
# non ecrases par une nouvelle) — d'ou ce garde-fou.
LIB_ATOMIC_PARSER_VERSION <- "2.1.1"

NUMBER_WORDS <- c(
  zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
  seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11, twelve = 12,
  thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16, seventeen = 17,
  eighteen = 18, nineteen = 19, twenty = 20
)
NUMBER_TOKEN <- paste0(
  "(", paste(names(NUMBER_WORDS), collapse = "|"), "|[0-9]+)"
)
VIRUS_TOKEN <- "(WPV[123]?|cVDPV[123])"
SOURCE_TOKEN <- "(case(?:s)?|environmental\\s+sample(?:s)?)"

# ------------------------------------------------------------
# Normalisation des mentions de virus AVANT extraction (jamais dans les
# donnees stockees : atomic_segment_text garde le texte original intact
# pour tracabilite). Fonde sur des formulations reelles confirmees sur
# polioeradication.org et dans des rapports OMS/CDC :
#   - noms en toutes lettres ("wild poliovirus type 1", "circulating
#     vaccine-derived poliovirus type 2") a cote des abreviations
#   - coquille connue "cVDVP" (D/P/V intervertis), vue y compris dans un
#     document officiel OMS
# Purement additif : elargit ce qui est RECONNU, ne change aucune
# semantique de classification.
# ------------------------------------------------------------
normalize_virus_mentions_in_text <- function(text) {
  text <- str_replace_all(text, "(?i)wild\\s+poliovirus\\s+type\\s*1\\b", "WPV1")
  text <- str_replace_all(text, "(?i)wild\\s+poliovirus\\s+type\\s*2\\b", "WPV2")
  text <- str_replace_all(text, "(?i)wild\\s+poliovirus\\s+type\\s*3\\b", "WPV3")
  text <- str_replace_all(text, "(?i)(?:circulating\\s+)?vaccine-derived\\s+poliovirus\\s+type\\s*1\\b", "cVDPV1")
  text <- str_replace_all(text, "(?i)(?:circulating\\s+)?vaccine-derived\\s+poliovirus\\s+type\\s*2\\b", "cVDPV2")
  text <- str_replace_all(text, "(?i)(?:circulating\\s+)?vaccine-derived\\s+poliovirus\\s+type\\s*3\\b", "cVDPV3")
  text <- str_replace_all(text, "(?i)cVDVP([123])", "cVDPV\\1")
  # Nettoyage : la substitution ci-dessus laisse parfois un parenthetique
  # redondant juste apres ("... WPV1 (WPV1) case ...") quand le texte
  # source ecrivait deja le nom complet suivi de son abreviation. On le
  # retire pour ne pas casser l'adjacence attendue par les gabarits.
  text <- str_replace_all(text, "(?i)\\b(WPV[123]|cVDPV[123])\\s*\\(\\s*\\1\\s*\\)", "\\1")
  text
}

#' Cherche des evenements nombre+virus+source dans un texte, en essayant
#' plusieurs gabarits confirmes dans des bulletins/rapports polio reels :
#'   A. NUMBER + VIRUS + (-positive)? + SOURCE
#      ex: "two WPV1-positive environmental samples", "four cVDPV2 cases"
#      (ordre confirme sur la page actuelle polioeradication.org)
#   B. NUMBER + SOURCE + "(tested )?positive for" + VIRUS
#      ex: "35 environmental samples positive for WPV1"
#      (ordre confirme dans des rapports de situation OMS)
#   C. NUMBER + "case(s) of" + VIRUS
#      ex: "307 cases of cVDPV2"
#      (ordre confirme dans des rapports de situation OMS)
#
# Les correspondances qui se chevauchent entre gabarits sont dedupliquees
# (la plus a gauche gagne). Un signal "loose" (nombre a proximite d'un
# virus ou d'une source, fenetre large) sert d'auto-controle : tout
# signal loose non couvert par une correspondance propre rend le segment
# ambigu -> revue humaine, jamais une extraction partielle devinee.
extract_events_from_segment <- function(text) {
  norm_text <- normalize_virus_mentions_in_text(text)

  templates <- list(
    list(
      pattern = paste0("(?i)\\b", NUMBER_TOKEN, "\\s+", VIRUS_TOKEN, "(?:[-\\s]+positive)?\\s+", SOURCE_TOKEN, "\\b"),
      roles = c("count", "virus", "source")
    ),
    list(
      pattern = paste0(
        "(?i)\\b", NUMBER_TOKEN, "\\s+", SOURCE_TOKEN,
        "\\s+(?:that\\s+)?(?:tested\\s+)?positive\\s+for\\s+", VIRUS_TOKEN, "\\b"
      ),
      roles = c("count", "source", "virus")
    ),
    list(
      pattern = paste0("(?i)\\b", NUMBER_TOKEN, "\\s+(case(?:s)?)\\s+of\\s+", VIRUS_TOKEN, "\\b"),
      roles = c("count", "source", "virus")
    )
  )

  clean_hits <- list()
  for (tmpl in templates) {
    locs <- str_locate_all(norm_text, tmpl$pattern)[[1]]
    if (nrow(locs) == 0) next
    mats <- str_match_all(norm_text, tmpl$pattern)[[1]]
    for (r in seq_len(nrow(mats))) {
      vals <- setNames(list(mats[r, 2], mats[r, 3], mats[r, 4]), tmpl$roles)
      clean_hits[[length(clean_hits) + 1L]] <- list(
        start = locs[r, "start"], end = locs[r, "end"],
        count_raw = vals$count, virus_raw = vals$virus, source_raw = vals$source
      )
    }
  }

  if (length(clean_hits) > 0) {
    ord <- order(vapply(clean_hits, function(x) x$start, numeric(1)))
    clean_hits <- clean_hits[ord]
    kept <- list()
    last_end <- -1L
    for (h in clean_hits) {
      if (h$start > last_end) {
        kept[[length(kept) + 1L]] <- h
        last_end <- h$end
      }
    }
    clean_hits <- kept
  }

  # Signal loose (fenetre large) : sert uniquement a detecter un
  # evenement "vu mais pas proprement extrait" ; jamais utilise pour
  # deviner une valeur.
  loose_pattern <- paste0(
    "(?i)\\b", NUMBER_TOKEN, "\\s+(?:[a-z'-]+\\s+){0,6}(?:", VIRUS_TOKEN, "|", SOURCE_TOKEN, ")\\b"
  )
  loose_locs <- str_locate_all(norm_text, loose_pattern)[[1]]

  unaccounted <- 0L
  if (nrow(loose_locs) > 0) {
    for (r in seq_len(nrow(loose_locs))) {
      covered <- length(clean_hits) > 0 && any(vapply(clean_hits, function(h) {
        loose_locs[r, "start"] <= h$end && h$start <= loose_locs[r, "end"]
      }, logical(1)))
      if (!covered) unaccounted <- unaccounted + 1L
    }
  }

  list(events = clean_hits, loose_mentions = nrow(loose_locs), unaccounted = unaccounted)
}

word_to_number <- function(x) {
  z <- tolower(trimws(x))
  digit <- suppressWarnings(as.integer(z))
  if (!is.na(digit)) return(digit)
  unname(NUMBER_WORDS[z])
}

find_country_colon_starts <- function(text) {
  # Motif : mot(s) commencant par une majuscule (Unicode, donc "C" comme
  # "O" accentue), jusqu'a ~45 caracteres, suivi d'un ":". Rejette les
  # candidats qui ressemblent a une phrase narrative (chiffres, mots
  # comme "reported"/"case"/"note"/"source").
  #
  # BUG CORRIGE (trouve par execution reelle) : la version precedente
  # n'acceptait que les lettres ASCII (A-Za-z), donc "Cote d'Ivoire:"
  # (avec un o accentue) faisait echouer la reconnaissance des le "o"
  # accentue et redemarrait a la lettre majuscule suivante -> "Ivoire:"
  # seul, perdant "Cote d'". \p{L}/\p{Lu} (lettre Unicode / lettre
  # majuscule Unicode) couvrent aussi les lettres accentuees.
  pattern <- "\\p{Lu}[\\p{L}.'\u2019\u2018\u2013\u2014\\-]*(?:[ ][\\p{L}.'\u2019\u2018\\-]+){0,5}\\s*:"
  matches <- stringr::str_locate_all(text, pattern)[[1]]
  if (nrow(matches) == 0) return(matches)

  candidates <- stringr::str_sub(text, matches[, "start"], matches[, "end"] - 1L)
  candidates <- str_replace(candidates, ":\\s*$", "")
  # NB: on n'utilise PAS looks_like_narrative_not_country() ici. Cette
  # fonction cherche des mots comme "new" n'importe ou dans le texte, ce
  # qui rejetterait a tort de vrais noms de pays comme "Papua New Guinea".
  # Un candidat de frontiere n'est rejete que s'il contient un chiffre
  # (jamais dans un nom de pays) ou s'il correspond exactement a un mot
  # generique de mise en page ("Note:", "Source:").
  keep <- !grepl("[0-9]", candidates) & !grepl("^(note|source)$", tolower(trimws(candidates)))
  matches[keep, , drop = FALSE]
}

segment_atomic_events <- function(raw_text, issue_date, source_url, row_id) {
  raw_text <- str_squish(as.character(raw_text))
  starts <- find_country_colon_starts(raw_text)

  segments <- list()
  if (nrow(starts) == 0) {
    # Pas de motif "Pays:" detecte : on ne devine pas, tout le texte part
    # en un seul segment "no_country_marker", qui echouera plus loin la
    # jointure au referentiel et partira en revue.
    segments[[1]] <- list(
      country_raw = NA_character_,
      segment_text = raw_text,
      atomic_segment_id = paste0(row_id, "_s1"),
      note = "no_country_colon_pattern_found"
    )
  } else {
    n <- nrow(starts)
    for (i in seq_len(n)) {
      seg_start <- starts[i, "start"]
      seg_end <- if (i < n) starts[i + 1L, "start"] - 1L else nchar(raw_text)
      seg_full <- str_sub(raw_text, seg_start, seg_end)
      colon_pos <- str_locate(seg_full, ":")[1, "end"]
      country_raw <- str_trim(str_sub(seg_full, 1, colon_pos - 1L))
      event_text <- str_trim(str_sub(seg_full, colon_pos + 1L, nchar(seg_full)))
      segments[[length(segments) + 1L]] <- list(
        country_raw = country_raw,
        segment_text = event_text,
        atomic_segment_id = paste0(row_id, "_s", i),
        note = NA_character_
      )
    }
  }

  out <- list()
  for (seg in segments) {
    if (is.na(seg$country_raw)) {
      out[[length(out) + 1L]] <- tibble(
        issue_date = issue_date, source_url = source_url,
        raw_text = raw_text, atomic_segment_id = seg$atomic_segment_id,
        atomic_segment_text = seg$segment_text, country_raw = NA_character_,
        virus_type = NA_character_, detection_source = NA_character_, count = NA_integer_,
        exclusion_reason = "no_country_colon_pattern_found"
      )
      next
    }

    extraction <- extract_events_from_segment(seg$segment_text)

    if (extraction$loose_mentions == 0L) {
      out[[length(out) + 1L]] <- tibble(
        issue_date = issue_date, source_url = source_url,
        raw_text = raw_text, atomic_segment_id = seg$atomic_segment_id,
        atomic_segment_text = seg$segment_text, country_raw = seg$country_raw,
        virus_type = NA_character_, detection_source = NA_character_, count = NA_integer_,
        exclusion_reason = "no_number_virus_mention_in_segment"
      )
      next
    }

    if (extraction$unaccounted > 0L) {
      out[[length(out) + 1L]] <- tibble(
        issue_date = issue_date, source_url = source_url,
        raw_text = raw_text, atomic_segment_id = seg$atomic_segment_id,
        atomic_segment_text = seg$segment_text, country_raw = seg$country_raw,
        virus_type = NA_character_, detection_source = NA_character_, count = NA_integer_,
        exclusion_reason = paste0(
          "ambiguous_segment_extraction (loose_mentions=", extraction$loose_mentions,
          ", clean_matches=", length(extraction$events),
          ", unaccounted=", extraction$unaccounted, ")"
        )
      )
      next
    }

    for (r in seq_along(extraction$events)) {
      ev <- extraction$events[[r]]
      count_val <- word_to_number(ev$count_raw)
      virus_val <- toupper(ev$virus_raw)
      virus_val <- if (str_detect(virus_val, "^CVDPV")) {
        str_replace(virus_val, "^CVDPV", "cVDPV")
      } else {
        virus_val
      }
      source_val <- if (str_detect(tolower(ev$source_raw), "environmental")) "Environmental" else "AFP"

      # BUG CORRIGE (trouve par execution reelle sur les 908 lignes, pas
      # par la relecture du code) : quand un MEME segment/pays rapporte
      # plusieurs evenements distincts, chaque evenement recevait le
      # MEME atomic_segment_id (celui du segment), donc le meme
      # issue_id en aval -> doublons dans ANALYTIC_ELIGIBLE et echec du
      # gate no_exact_duplicates_in_analytic. Chaque evenement extrait a
      # maintenant son propre identifiant.
      out[[length(out) + 1L]] <- tibble(
        issue_date = issue_date, source_url = source_url,
        raw_text = raw_text, atomic_segment_id = paste0(seg$atomic_segment_id, "_e", r),
        atomic_segment_text = seg$segment_text, country_raw = seg$country_raw,
        virus_type = virus_val, detection_source = source_val,
        count = as.integer(count_val), exclusion_reason = NA_character_
      )
    }
  }

  bind_rows(out)
}
