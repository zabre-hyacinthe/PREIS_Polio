# =========================================================
# R/60_email.R
# PREIS_Polio_FV — Moteur email (blastula)
# =========================================================

# =========================================================
# R/60_email.R
# =========================================================

suppressPackageStartupMessages({
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop("Package 'blastula' is required in R/60_email.R")
  }
})

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
