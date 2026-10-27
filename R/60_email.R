# =========================================================
# R/60_email.R
# PREIS_Polio_FV — Moteur email (blastula)
# =========================================================

# =========================================================
# R/60_email.R
# =========================================================

# Ne pas exiger blastula au chargement du pipeline. En mode dry-run, aucun
# moteur SMTP n'est nécessaire. Pour un envoi réel, la dépendance est contrôlée
# juste avant son utilisation avec un message d'erreur exploitable.
require_blastula_for_send <- function() {
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop(
      paste0(
        "Package R 'blastula' absent: envoi email impossible. ",
        "Le workflow GitHub doit executer setup-r-dependencies avant ",
        "R/120_run_polio_cloud.R."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

get_email_env <- function() {
  list(
    smtp_user  = Sys.getenv("SMTP_USER", ""),
    smtp_pass  = Sys.getenv("SMTP_PASS", ""),
    smtp_host  = Sys.getenv("SMTP_HOST", "smtp.gmail.com"),
    smtp_port  = as.integer(Sys.getenv("SMTP_PORT", "465")),
    alert_from = Sys.getenv("ALERT_FROM", Sys.getenv("SMTP_USER", "")),
    dry_run_env = tolower(Sys.getenv("PREIS_DRY_RUN", "false"))
  )
}

is_dry_run_email <- function(send_now = TRUE) {
  env <- get_email_env()
  if (!isTRUE(send_now)) return(TRUE)
  if (env$dry_run_env %in% c("1", "true", "yes", "y")) return(TRUE)
  FALSE
}

# ---------------------------------------------------------
# Garde-fou destinataires (liste blanche, "fail-closed")
# Ajoute le 27 oct. 2026. Un envoi REEL n'est possible que vers une adresse
# confirmee par l'auteur dans alert_recipients.csv (colonne owner_confirmed
# renseignee). Fichier ou colonne absents, ou adresse non confirmee :
# AUCUN envoi (statut "blocked_not_authorized"). Les adresses ne sont jamais
# ecrites dans les messages de ce garde-fou.
# ---------------------------------------------------------
recipients_allowlist_path <- function() {
  p <- Sys.getenv("PREIS_RECIPIENTS_FILE", "")
  if (nzchar(p)) return(p)
  file.path(getwd(), "dashboard", "data", "dashboard", "alert_recipients.csv")
}

is_recipient_authorized <- function(to, path = recipients_allowlist_path()) {
  to_norm <- tolower(trimws(as.character(to)))
  if (length(to_norm) != 1L || is.na(to_norm) || !nzchar(to_norm)) return(FALSE)
  if (!file.exists(path)) return(FALSE)
  al <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                    colClasses = "character", fileEncoding = "UTF-8-BOM"),
    error = function(e) NULL
  )
  if (is.null(al)) return(FALSE)
  names(al) <- tolower(trimws(names(al)))
  if (!all(c("email", "owner_confirmed") %in% names(al))) return(FALSE)
  em <- tolower(trimws(al$email))
  oc <- tolower(trimws(al$owner_confirmed))
  ok <- !is.na(oc) & nzchar(oc) & !(oc %in% c("false", "0", "no", "non", "n"))
  any(!is.na(em) & em == to_norm & ok)
}

send_email_safely <- function(
    to,
    subject,
    body,
    html = NULL,
    send_now = TRUE,
    cc = NULL,
    bcc = NULL
) {
  env <- get_email_env()
  
  if (length(to) == 0 || is.na(to) || !nzchar(trimws(to))) {
    stop("send_email_safely(): recipient 'to' is missing.")
  }
  
  if (!nzchar(trimws(subject))) {
    stop("send_email_safely(): subject is missing.")
  }
  
  msg_body <- if (!is.null(html) && nzchar(html)) html else body
  if (is.null(msg_body) || !nzchar(trimws(msg_body))) {
    stop("send_email_safely(): body/html content is empty.")
  }
  
  # Garde-fou liste blanche (to + cc + bcc)
  all_rcpt <- c(to, cc, bcc)
  all_rcpt <- all_rcpt[!is.na(all_rcpt) & nzchar(trimws(all_rcpt))]
  rcpt_ok <- all(vapply(all_rcpt, is_recipient_authorized, logical(1)))
  if (!rcpt_ok) {
    if (is_dry_run_email(send_now = send_now)) {
      message("[EMAIL] NOTE (dry_run): at least one recipient is not confirmed ",
              "in the allowlist; a real send would be BLOCKED.")
    } else {
      message("[EMAIL] BLOCKED: recipient not confirmed in the allowlist -> no email sent.")
      return(list(
        success = FALSE,
        status = "blocked_not_authorized",
        to = to,
        subject = subject,
        error = "blocked_not_authorized: recipient not confirmed by owner in alert_recipients.csv"
      ))
    }
  }

  if (is_dry_run_email(send_now = send_now)) {
    message("[EMAIL] dry_run -> no email sent to: ", to)
    return(list(
      success = TRUE,
      status = "dry_run",
      to = to,
      subject = subject
    ))
  }
  
  if (!nzchar(env$smtp_user)) stop("SMTP_USER is missing.")
  if (!nzchar(env$smtp_pass)) stop("SMTP_PASS is missing.")
  if (!nzchar(env$alert_from)) stop("ALERT_FROM is missing.")

  require_blastula_for_send()
  
  # Make sure the password is visible to blastula through an env var
  Sys.setenv(SMTP_PASS = env$smtp_pass)
  
  email_obj <- blastula::compose_email(
    body = blastula::md(msg_body)
  )
  
  res <- tryCatch(
    {
      blastula::smtp_send(
        email = email_obj,
        from = env$alert_from,
        to = to,
        cc = cc,
        bcc = bcc,
        subject = subject,
        credentials = blastula::creds_envvar(
          user = env$smtp_user,
          pass_envvar = "SMTP_PASS",
          host = env$smtp_host,
          port = env$smtp_port,
          use_ssl = TRUE
        )
      )
      
      message("[EMAIL] sent successfully to: ", to)
      
      list(
        success = TRUE,
        status = "sent",
        to = to,
        subject = subject
      )
    },
    error = function(e) {
      message("[EMAIL] failed for ", to, " -> ", e$message)
      list(
        success = FALSE,
        status = "failed",
        to = to,
        subject = subject,
        error = e$message
      )
    }
  )
  
  res
}

stopifnot(exists("send_email_safely", mode = "function"))

# ---------------------------------------------------------
# Alias de compatibilite (scripts legacy)
# ---------------------------------------------------------
preis_send_email <- function(
    to,
    subject,
    body_text,
    attachments = NULL,
    dry_run     = TRUE
) {
  if (!is.null(attachments) && length(attachments) > 0) {
    message("[EMAIL] Pieces jointes ignorees (non supportees par blastula >= 0.3)")
  }
  send_email_safely(
    to       = to,
    subject  = subject,
    body     = body_text,
    send_now = !isTRUE(dry_run)
  )
}

stopifnot(exists("preis_send_email", mode = "function"))
