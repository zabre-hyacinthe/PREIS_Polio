#!/usr/bin/env Rscript
# ============================================================
# tests/test_count_fix.R
# PREIS POLIO - tests de non-regression du comptage atomique
# (R/05_prepare_polio_alert_input.R, correctif du 27 oct. 2026).
#
# Usage (depuis la racine du projet D:\PREIS_Polio_FV) :
#   Rscript tests/test_count_fix.R
#
# Aucun acces reseau, aucun fichier du projet n'est modifie.
# Verifie que :
#   - le comptage somme bien cas humains + echantillons environnementaux
#     (ancien defaut : premier nombre seulement, "1" par defaut) ;
#   - un texte ambigu / sans nombre / multi-pays donne un comptage NA et
#     count_status = "review" - jamais 1 - tout en conservant le signal ;
#   - un texte melant WPV et cVDPV n'est jamais classe en dessous de WPV ;
#   - sans les modules de comptage, aucune ligne ne recoit un nombre invente.
# ============================================================
suppressPackageStartupMessages({library(dplyr); library(stringr); library(tibble)})

if (!file.exists("R/05_prepare_polio_alert_input.R")) {
  stop("Lancer ce test depuis la racine du projet (dossier contenant R/ et config/).")
}

# R/05 charge rvest/xml2 (lecture de la page) : inutiles ici. Si absents de
# cette machine, on charge une copie temporaire sans ces deux library().
src <- "R/05_prepare_polio_alert_input.R"
if (!requireNamespace("rvest", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
  l <- readLines(src, warn = FALSE)
  l <- l[!grepl("^\\s*library\\((rvest|xml2)\\)\\s*$", l)]
  src <- tempfile(fileext = ".R"); writeLines(l, src)
}
sys.source(src, envir = .GlobalEnv)
stopifnot(.load_count_parser(getwd()))

n_pass <- 0L; n_fail <- 0L
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
}
cb <- function(txt) .count_bullet(txt, "2026-10-21", "test", "t")

# --- 1. bulletins reels du 21 oct. 2026 -------------------------------------
r <- cb("Chad : one cVDPV2 case and one cVDPV2-positive environmental sample")
check("1a. Chad : 1 cas + 1 echantillon (ancien code : 1)", nrow(r) == 1 && r$n_cases == 1 && r$n_env_samples == 1 && r$count_status == "parsed")
r <- cb("Nigeria : one cVDPV3 case and two cVDPV3-positive environmental samples")
check("1b. Nigeria : 1 cas + 2 echantillons (ancien code : 1)", r$n_cases == 1 && r$n_env_samples == 2 && r$virus_type == "cVDPV3")
r <- cb("DR Congo : six cVDPV2 cases")
check("1c. RDC : 6 cas, 0 echantillon", r$n_cases == 6 && r$n_env_samples == 0)
r <- cb("Madagascar : one cVDPV1-positive environmental sample")
check("1d. Madagascar : 0 cas, 1 echantillon", r$n_cases == 0 && r$n_env_samples == 1)

# --- 2. plusieurs virus : une ligne par virus --------------------------------
r <- cb("Mozambique: three WPV1 cases and two cVDPV1 cases")
check("2a. WPV1 + cVDPV1 : deux lignes", nrow(r) == 2 && setequal(r$virus_type, c("WPV1", "cVDPV1")))
check("2b. comptes par virus (3 WPV1, 2 cVDPV1)", r$n_cases[r$virus_type == "WPV1"] == 3 && r$n_cases[r$virus_type == "cVDPV1"] == 2)

# --- 3. cas ambigus : NA, jamais 1, signal conserve --------------------------
r <- cb("Niger : cVDPV2 positive environmental samples detected")
check("3a. sans nombre -> review, comptage NA", nrow(r) == 1 && r$count_status == "review" && is.na(r$n_cases) && is.na(r$n_env_samples))
check("3b. sans nombre -> virus conserve (cVDPV2)", r$virus_type == "cVDPV2")
r <- cb("Mozambique : WPV1 detected, number pending")
check("3c. WPV1 sans nombre -> review, virus WPV1 conserve", r$count_status == "review" && r$virus_type == "WPV1")
r <- cb("Angola : one cVDPV2 case. Zambia : two cVDPV2 cases")
check("3d. texte multi-pays -> review (raison explicite)", all(r$count_status == "review") && any(grepl("multiple_segments", r$count_review_reason)))
r <- cb("")
check("3e. texte vide -> review, pas d'erreur", all(r$count_status == "review"))

# --- 4. aucun comptage de remplacement ---------------------------------------
many <- bind_rows(lapply(c("Niger : cVDPV2 positive environmental samples detected",
                           "Mozambique : WPV1 detected, number pending", ""), cb))
check("4. aucune ligne review n'a un comptage non NA", all(is.na(many$n_cases) & is.na(many$n_env_samples)))

# --- 5. .apply_atomic_counts : colonnes de sortie ----------------------------
df <- tibble(
  raw_bullet = c("Chad : one cVDPV2 case and one cVDPV2-positive environmental sample",
                 "Niger : cVDPV2 positive environmental samples detected"),
  summary_text = c("one cVDPV2 case and one cVDPV2-positive environmental sample",
                   "cVDPV2 positive environmental samples detected"),
  virus_type = "x", signal_type = "x", event_type = "x", count = NA_integer_
)
o <- .apply_atomic_counts(df, TRUE, "2026-10-21", "test", "issue_test")
check("5a. count = cas + echantillons pour une ligne parsed", o$count[o$count_status == "parsed"] == 2L)
check("5b. count = NA pour une ligne review", is.na(o$count[o$count_status == "review"]))
check("5c. event_type derive du comptage (Case + Environmental)", o$event_type[o$count_status == "parsed"] == "Case + Environmental")
check("5d. ligne review : signal conserve via mots-cles (Environmental)", o$event_type[o$count_status == "review"] == "Environmental")
check("5e. parser_version renseigne", all(nzchar(o$parser_version)))

# --- 6. modules de comptage absents ------------------------------------------
o2 <- .apply_atomic_counts(df, FALSE, "2026-10-21", "test", "issue_test")
check("6. sans module : toutes les lignes review, comptage NA, aucun 1", all(o2$count_status == "review") && all(is.na(o2$count)))

cat("\n", strrep("=", 60), "\nBILAN: ", n_pass, " / ", n_pass + n_fail, " tests passes\n", sep = "")
if (n_fail > 0) { cat("ECHEC.\n"); quit(status = 1) } else cat("Tous les tests sont passes.\n")
