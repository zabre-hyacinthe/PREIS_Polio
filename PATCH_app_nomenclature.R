# PATCH_app_nomenclature.R
# PREIS_Polio_FV - Corrige dashboard/app.R in-place
# setwd("D:/PREIS_Polio_FV") puis source("PATCH_app_nomenclature.R")

fp <- "dashboard/app.R"

if (!file.exists(fp)) {
  stop("Fichier introuvable : ", fp,
       "\nVerifie que getwd() == 'D:/PREIS_Polio_FV'", call. = FALSE)
}

lines <- readLines(fp, warn = FALSE, encoding = "UTF-8")
cat("Lu :", length(lines), "lignes depuis", fp, "\n")

n <- 0L

subs <- list(
  c("PREIS_POLIO",           "PREIS_Polio_FV"),
  c("PREIS POLIO",           "PREIS Polio FV"),
  c("preis_polio_filtered_", "preis_polio_fv_filtered_")
)

for (s in subs) {
  idx <- which(grepl(s[1], lines, fixed = TRUE))
  if (length(idx) > 0L) {
    lines[idx] <- gsub(s[1], s[2], lines[idx], fixed = TRUE)
    cat("[OK]", length(idx), "ligne(s) :", s[1], "->", s[2], "\n")
    n <- n + length(idx)
  }
}

if (n > 0L) {
  con <- file(fp, open = "wb")
  writeLines(lines, con = con)
  close(con)
  cat("\n", n, "correction(s) sauvegardee(s).\n")
} else {
  cat("\nFichier deja propre - rien a corriger.\n")
}

bad <- c("PREIS_POLIO", "PREIS POLIO", "preis_polio")
remain <- unlist(lapply(bad, function(p) {
  idx <- which(grepl(p, lines, fixed = TRUE))
  if (length(idx) > 0L) paste0("L.", idx, " [", p, "]") else character(0L)
}))

if (length(remain) == 0L) {
  cat("OK - dashboard/app.R est 100% propre.\n")
} else {
  cat("ATTENTION :", length(remain), "ligne(s) restante(s) :\n")
  cat(paste(remain, collapse = "\n"), "\n")
}
