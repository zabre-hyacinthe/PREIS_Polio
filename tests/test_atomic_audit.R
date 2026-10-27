#!/usr/bin/env Rscript
# ============================================================
# tests/test_atomic_audit.R
# PREIS POLIO — tests reproductibles pour lib_country_normalize.R et
# lib_atomic_parser.R (segmentation atomique, dossier de reprise du
# 01/08/2026).
#
# Usage (depuis la racine du projet D:\PREIS_Polio_FV) :
#   Rscript tests/test_atomic_audit.R
#
# Couverture actuelle : 16 des 22 cas demandes dans le dossier de reprise
# (39 assertions au total).
#   1.  phrase simple, un pays/virus/source/nombre
#   2.  paragraphe multi-pays (regression reelle Afghanistan/Tchad/PNG)
#   3.  plusieurs virus dans le meme texte
#   4.  AFP et environnemental dans le meme texte
#   4b. meme segment, plusieurs evenements -> identifiants tous distincts
#       (regression reelle trouvee lors du 1er run sur les 908 lignes)
#   5.  plusieurs nombres dans un texte, distincts par pays
#   6.  nombres exprimes en lettres
#   7.  faux pays narratif "Five cases ... were reported" (cas reel documente)
#   8.  faux pays narratif "No new WPV1 ... this week ..." (cas reel documente)
#   9.  variantes accentuees (Cote d'Ivoire / Cote d'Ivoire avec accent)
#   10. apostrophes/parentheses/casse (Dr Congo, DRC, (DR Congo)) + "Congo"
#       seul volontairement NON resolu
#   11. valeurs manquantes (raw_text NA / vide)
#   12. count nul explicite dans le texte ("zero cases")
#   13. determinisme (base du controle de doublon exact du script principal ;
#       le doublon exact sur donnees reelles est verifie separement en
#       integration, voir outputs/atomic_audit_v2 apres --mode=audit)
#   14. gabarits d'extraction supplementaires (ajoutes apres le run reel du
#       07/08/2026, qui a exclu ~25% des candidats en
#       ambiguous_segment_extraction) : ordre inverse "X positive for
#       VIRUS", "X cases of VIRUS", noms de virus en toutes lettres,
#       coquille connue cVDVP, texte descriptif additionnel, et texte
#       cumulatif (doit rester exclu, jamais compte comme evenement)
#
# RESTE A FAIRE (non couvert ici) :
#   - pays valide mais nombre/virus/source provenant d'un autre segment
#     dans un texte ambigu construit expres (au-dela de la regression #2)
#   - deux lignes distinctes devenant identiques apres normalisation
#   - count negatif : non applicable au niveau texte (aucun gabarit ne
#     peut produire un nombre negatif depuis du texte source ; le gate
#     invalid_or_negative_count du script principal reste une securite
#     defensive, a tester au niveau integration si le schema d'entree
#     change un jour)
#   - date invalide dans issue_date (verifie au niveau du script
#     principal, pas de segment_atomic_events lui-meme)
#   - panne reseau Wayback / source locale avec reseau desactive
#     (concerne --mode=collect, hors scope de ce fichier)
# ============================================================

suppressPackageStartupMessages({
  library(stringr)
  library(tibble)
  library(dplyr)
})

this_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(file_arg)) dirname(normalizePath(file_arg[1])) else getwd()
}, error = function(e) getwd())
project_root <- normalizePath(file.path(this_dir, ".."), mustWork = FALSE)
if (!dir.exists(file.path(project_root, "R"))) project_root <- getwd()

source(file.path(project_root, "R", "lib_country_normalize.R"), local = FALSE)
source(file.path(project_root, "R", "lib_atomic_parser.R"), local = FALSE)

country_ref <- load_country_reference(file.path(project_root, "config", "country_reference_v1.csv"))
lookup_country_test <- function(raw_name) {
  key <- country_join_key(raw_name)
  hit <- country_ref$lookup[country_ref$lookup$key == key, , drop = FALSE]
  if (nrow(hit) == 0) NULL else as.list(hit[1, ])
}

results <- list()
check <- function(label, condition) {
  passed <- isTRUE(condition)
  results[[length(results) + 1L]] <<- data.frame(test = label, passed = passed, stringsAsFactors = FALSE)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", label))
}

# ------------------------------------------------------------
# 1. Phrase simple
# ------------------------------------------------------------
seg <- segment_atomic_events("Nigeria: four cVDPV2 cases", "2026-01-05", "http://example.org", "t1")
check("1. phrase simple -> 1 evenement", nrow(seg) == 1)
check("1. phrase simple -> pays correct", seg$country_raw[1] == "Nigeria")
check("1. phrase simple -> virus correct", seg$virus_type[1] == "cVDPV2")
check("1. phrase simple -> source correcte", seg$detection_source[1] == "AFP")
check("1. phrase simple -> nombre correct", seg$count[1] == 4)

# ------------------------------------------------------------
# 2. Regression reelle : paragraphe multi-pays (Afghanistan/Tchad/PNG).
#    Chaque champ doit provenir du meme segment, jamais du precedent.
# ------------------------------------------------------------
multi <- paste0(
  "Afghanistan: two WPV1 cases. ",
  "Chad: two cVDPV2-positive environmental samples. ",
  "Papua New Guinea: seven cVDPV2-positive environmental samples."
)
seg <- segment_atomic_events(multi, "2025-10-09", "http://example.org", "t2")
seg_ok <- seg %>% filter(is.na(exclusion_reason))
check("2. multi-pays -> 3 evenements distincts", nrow(seg_ok) == 3)
afg <- seg_ok %>% filter(country_raw == "Afghanistan")
tcd <- seg_ok %>% filter(country_raw == "Chad")
png <- seg_ok %>% filter(country_raw == "Papua New Guinea")
check("2. Afghanistan = WPV1 / AFP / 2", nrow(afg) == 1 && afg$virus_type[1] == "WPV1" && afg$detection_source[1] == "AFP" && afg$count[1] == 2)
check("2. Chad = cVDPV2 / Environmental / 2", nrow(tcd) == 1 && tcd$virus_type[1] == "cVDPV2" && tcd$detection_source[1] == "Environmental" && tcd$count[1] == 2)
check("2. Papua New Guinea = cVDPV2 / Environmental / 7", nrow(png) == 1 && png$virus_type[1] == "cVDPV2" && png$detection_source[1] == "Environmental" && png$count[1] == 7)
check("2. aucun melange (pas de ligne Afghanistan avec count=7)", nrow(afg %>% filter(count == 7)) == 0)

# ------------------------------------------------------------
# 3. Plusieurs virus dans le meme texte (pays unique)
# ------------------------------------------------------------
seg <- segment_atomic_events(
  "Pakistan: one WPV1 case and two cVDPV2-positive environmental samples.",
  "2026-01-05", "http://example.org", "t3"
)
seg_ok3 <- seg %>% filter(is.na(exclusion_reason))
check("3. multi-virus meme pays -> 2 evenements", nrow(seg_ok3) == 2)
check("3. contient WPV1", any(seg_ok3$virus_type == "WPV1"))
check("3. contient cVDPV2", any(seg_ok3$virus_type == "cVDPV2"))

# ------------------------------------------------------------
# 4. AFP et environnemental dans le meme texte (verification des sources
#    sur le meme cas que le test 3)
# ------------------------------------------------------------
check("4. contient une ligne AFP", any(seg_ok3$detection_source == "AFP"))
check("4. contient une ligne Environmental", any(seg_ok3$detection_source == "Environmental"))

# ------------------------------------------------------------
# 4b. REGRESSION REELLE (trouvee en executant sur les 908 vraies lignes,
#     pas par relecture de code) : un meme segment/pays qui rapporte
#     plusieurs evenements distincts recevait le meme atomic_segment_id
#     pour chacun -> doublons d'issue_id en aval -> gate
#     no_exact_duplicates_in_analytic en echec -> BLOCKED_QC_FAILURE.
#     Chaque evenement extrait doit avoir un identifiant unique, meme
#     s'il partage son segment source avec d'autres evenements.
# ------------------------------------------------------------
check("4b. plusieurs evenements du meme segment -> atomic_segment_id tous distincts",
      length(unique(seg_ok3$atomic_segment_id)) == nrow(seg_ok3))

# ------------------------------------------------------------
# 5. Plusieurs nombres dans un texte, distincts par pays (cas 2) :
#    verification qu'aucun nombre n'est duplique/perdu.
# ------------------------------------------------------------
check("5. plusieurs nombres -> valeurs distinctes preservees (2,2,7)",
      setequal(seg_ok$count, c(2, 2, 7)) && nrow(seg_ok) == 3)

# ------------------------------------------------------------
# 6. Nombres exprimes en lettres
# ------------------------------------------------------------
seg <- segment_atomic_events("Somalia: eleven cVDPV1 cases", "2026-01-05", "http://example.org", "t6")
check("6. nombre en lettres (eleven -> 11)", seg$count[1] == 11 && is.na(seg$exclusion_reason[1]))

# ------------------------------------------------------------
# 7-8. Faux pays narratifs (cas reels documentes dans le dossier de reprise)
# ------------------------------------------------------------
seg <- segment_atomic_events(
  "Five cases of circulating vaccine-derived poliovirus type 2 (cVDPV2) were reported",
  "2026-01-05", "http://example.org", "t7"
)
check("7. faux pays narratif 'Five cases...' -> aucune ligne eligible", nrow(seg %>% filter(is.na(exclusion_reason))) == 0)

seg <- segment_atomic_events(
  paste0(
    "No new WPV1 or cVDPV2 viruses were reported this week. There is one WPV1 ",
    "case reported in 2022 and four in 2021. The total number of cVDPV2 cases remains 43."
  ),
  "2026-01-05", "http://example.org", "t8"
)
check("8. faux pays narratif (paragraphe complet) -> aucune ligne eligible", nrow(seg %>% filter(is.na(exclusion_reason))) == 0)

# ------------------------------------------------------------
# 9. Variantes accentuees
# ------------------------------------------------------------
check("9. cle identique Cote d'Ivoire accentue / non accentue",
      country_join_key("C\u00f4te d'Ivoire") == country_join_key("Cote d'Ivoire"))
hit1 <- lookup_country_test("C\u00f4te d'Ivoire")
hit2 <- lookup_country_test("Cote D\u2019Ivoire")
check("9. les deux variantes matchent le referentiel", !is.null(hit1) && !is.null(hit2))
check("9. meme pays canonique pour les deux variantes",
      !is.null(hit1) && !is.null(hit2) && identical(hit1$country_canonical, hit2$country_canonical))

# ------------------------------------------------------------
# 10. Apostrophes / parentheses / casse (Dr Congo, DRC, (DR Congo)) et
#     "Congo" seul volontairement NON resolu (ambigu)
# ------------------------------------------------------------
for (variant in c("Dr Congo", "DRC", "Democratic Republic of the Congo (DR Congo)", "DR CONGO")) {
  hit <- lookup_country_test(variant)
  check(
    paste0("10. variante RDC reconnue: '", variant, "'"),
    !is.null(hit) && identical(hit$country_canonical, "Democratic Republic of the Congo")
  )
}
check("10. 'Congo' seul reste NON resolu (ambigu, ne doit jamais matcher automatiquement)",
      is.null(lookup_country_test("Congo")))

# ------------------------------------------------------------
# 11. Valeurs manquantes
# ------------------------------------------------------------
seg <- segment_atomic_events(NA_character_, "2026-01-05", "http://example.org", "t11")
check("11. raw_text NA -> exclu proprement, pas d'erreur", nrow(seg) >= 1 && all(!is.na(seg$exclusion_reason)))

seg <- segment_atomic_events("", "2026-01-05", "http://example.org", "t11b")
check("11b. raw_text vide -> exclu proprement", nrow(seg) >= 1 && all(!is.na(seg$exclusion_reason)))

# ------------------------------------------------------------
# 12. Count nul explicite dans le texte source
# ------------------------------------------------------------
seg <- segment_atomic_events("Niger: zero cVDPV2 cases", "2026-01-05", "http://example.org", "t12")
check("12. 'zero cases' -> count = 0, non exclu pour ce motif", seg$count[1] == 0 && is.na(seg$exclusion_reason[1]))

# ------------------------------------------------------------
# 13. Determinisme (base du controle de doublon exact du script principal)
# ------------------------------------------------------------
seg_a <- segment_atomic_events("Ghana: three cVDPV2 cases", "2026-01-05", "http://example.org", "t13a")
seg_b <- segment_atomic_events("Ghana: three cVDPV2 cases", "2026-01-05", "http://example.org", "t13a")
check("13. segmentation deterministe (memes entrees -> memes resultats)",
      identical(seg_a$country_raw, seg_b$country_raw) &&
      identical(seg_a$virus_type, seg_b$virus_type) &&
      identical(seg_a$count, seg_b$count))

# ------------------------------------------------------------
# 14. Gabarits d'extraction supplementaires, ajoutes apres un run reel sur
#     les 908 lignes ayant exclu ~25% des candidats pour
#     "ambiguous_segment_extraction". Formulations confirmees par
#     recherche web sur polioeradication.org et des rapports OMS/CDC
#     (pas verifiees contre le texte brut exact de ce projet - a confirmer
#     avec de vrais echantillons de REVIEW_EXCLUDED.csv).
# ------------------------------------------------------------
seg <- segment_atomic_events("Pakistan: 35 environmental samples positive for WPV1 were reported", "2026-01-05", "http://example.org", "t14a")
check("14a. ordre inverse 'X samples positive for VIRUS'", seg$virus_type[1] == "WPV1" && seg$detection_source[1] == "Environmental" && seg$count[1] == 35 && is.na(seg$exclusion_reason[1]))

seg <- segment_atomic_events("Afghanistan: 307 cases of cVDPV2 were reported", "2026-01-05", "http://example.org", "t14b")
check("14b. ordre inverse 'X cases of VIRUS'", seg$virus_type[1] == "cVDPV2" && seg$detection_source[1] == "AFP" && seg$count[1] == 307 && is.na(seg$exclusion_reason[1]))

seg <- segment_atomic_events("Nigeria: one wild poliovirus type 1 (WPV1) case was reported", "2026-01-05", "http://example.org", "t14c")
check("14c. nom de virus en toutes lettres avec abreviation entre parentheses", seg$virus_type[1] == "WPV1" && is.na(seg$exclusion_reason[1]))

seg <- segment_atomic_events("Chad: two circulating vaccine-derived poliovirus type 2 (cVDPV2) positive environmental samples were confirmed", "2026-01-05", "http://example.org", "t14d")
check("14d. nom complet cVDPV + connecteur 'positive' sans trait d'union", seg$virus_type[1] == "cVDPV2" && seg$detection_source[1] == "Environmental" && seg$count[1] == 2 && is.na(seg$exclusion_reason[1]))

seg <- segment_atomic_events("Somalia: three environmental samples positive for cVDVP2 were reported", "2026-01-05", "http://example.org", "t14e")
check("14e. coquille connue cVDVP (D/P/V intervertis) corrigee en cVDPV", seg$virus_type[1] == "cVDPV2" && is.na(seg$exclusion_reason[1]))

seg <- segment_atomic_events("Mali: One cVDPV1 case was reported this week from Segou province, with onset of paralysis on 12 May 2026 (most recent case).", "2026-01-05", "http://example.org", "t14f")
check("14f. texte descriptif apres l'evenement (date, province) n'empeche pas l'extraction", seg$virus_type[1] == "cVDPV1" && seg$count[1] == 1 && is.na(seg$exclusion_reason[1]))

# ------------------------------------------------------------
# 14g. Texte cumulatif (annonce hebdo + total annuel dans le meme
#      segment) : comportement volontairement CONSERVATEUR. On exclut le
#      segment entier plutot que de risquer de compter le total annuel
#      comme un nouvel evenement (regle explicite du projet). Le
#      compromis : on perd aussi le chiffre hebdo legitime dans ce cas ;
#      recuperation possible en revue humaine via REVIEW_EXCLUDED.csv.
# ------------------------------------------------------------
seg <- segment_atomic_events(
  "One cVDPV1 case was reported this week from Borikhamxay province, with onset of paralysis on 31 May 2026. Three cases have been reported in 2026.",
  "2026-06-05", "http://example.org", "t14g"
)
check("14g. texte cumulatif -> segment entier exclu (jamais le total compte comme evenement)",
      nrow(seg %>% filter(is.na(exclusion_reason))) == 0)

# ------------------------------------------------------------
# 15. Textes REELS ET ACTUELS recuperes directement sur
#     polioeradication.org/about-polio/polio-this-week/ le 07/08/2026 (la
#     page meme que scrape ce projet). Fragments factuels courts,
#     structurellement representatifs, non redistribues en substance.
# ------------------------------------------------------------
seg <- segment_atomic_events(
  "Democratic Republic of the Congo: 12 cVDPV2 cases and one cVDPV2-positive environmental sample",
  "2026-08-05", "http://example.org", "t15a"
)
seg_ok <- seg %>% filter(is.na(exclusion_reason))
check("15a. DRC reel -> 2 evenements (meme virus, sources differentes)", nrow(seg_ok) == 2)
check("15a. DRC reel -> une ligne AFP count=12, une Environmental count=1",
      any(seg_ok$detection_source == "AFP" & seg_ok$count == 12) &&
      any(seg_ok$detection_source == "Environmental" & seg_ok$count == 1))

seg <- segment_atomic_events("Nigeria: one cVDPV2 case and one cVDPV3 case", "2026-08-05", "http://example.org", "t15b")
seg_ok <- seg %>% filter(is.na(exclusion_reason))
check("15b. Nigeria reel -> 2 evenements (virus differents, meme source)", nrow(seg_ok) == 2)
check("15b. Nigeria reel -> contient cVDPV2 et cVDPV3", all(c("cVDPV2","cVDPV3") %in% seg_ok$virus_type))

# ------------------------------------------------------------
# 16. REGRESSION REELLE (trouvee sur les 908 vraies lignes le 07/08/2026) :
#     le regex de frontiere "Pays:" n'acceptait que les lettres ASCII.
#     "Cote d'Ivoire:" (o accentue) faisait echouer la detection au "o"
#     accentue et redemarrait a la majuscule suivante -> "Ivoire:" seul,
#     perdant "Cote d'" (29 lignes concernees dans les vraies donnees).
# ------------------------------------------------------------
seg <- segment_atomic_events("C\u00f4te d'Ivoire: four cVDPV2 cases", "2026-01-05", "http://example.org", "t16a")
check("16a. Cote d'Ivoire accentue -> country_raw complet (pas tronque en 'Ivoire')",
      seg$country_raw[1] == "C\u00f4te d'Ivoire")

hit <- (function(x) {
  key <- country_join_key(x)
  ref <- load_country_reference("config/country_reference_v1.csv")
  h <- ref$lookup[ref$lookup$key == key, , drop = FALSE]
  if (nrow(h) == 0) NA_character_ else h$country_canonical[1]
})("Central African Rep")
check("16b. abreviation 'Central African Rep' reconnue dans le referentiel", identical(hit, "Central African Republic"))

# ------------------------------------------------------------
# BILAN
# ------------------------------------------------------------
all_results <- do.call(rbind, results)
n_pass <- sum(all_results$passed)
n_total <- nrow(all_results)
cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("BILAN:", n_pass, "/", n_total, "tests passes\n")
if (n_pass < n_total) {
  cat("ECHECS:\n")
  print(all_results$test[!all_results$passed])
  quit(status = 1, save = "no")
}
cat("Tous les tests sont passes.\n")
