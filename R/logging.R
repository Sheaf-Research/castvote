#' @noRd
log_level_numbers <- c(
  "FATAL" = 100L,
  "ERROR" = 200L,
  "WARN" = 300L,
  "INFO" = 400L,
  "DEBUG" = 500L,
  "TRACE" = 600L
)

#' Logging level
#'
#' castvote logs through the logger package under the `"castvote"` namespace.
#' Logging is quiet by default: only warnings and above are emitted. Raise the
#' level to `"INFO"` or `"DEBUG"` to follow progress. The level is also read
#' from the `CASTVOTE_LOG_LEVEL` environment variable or the
#' `castvote.log_level` option when the package loads.
#'
#' @param level A log level, one of `"TRACE"`, `"DEBUG"`, `"INFO"`, `"WARN"`,
#'   `"ERROR"`, or `"FATAL"`, matched case-insensitively. `NULL` (the default)
#'   returns the current level.
#' @return When reading, the current level as a string. When setting, the level
#'   invisibly.
#' @export
castvote_log_level <- function(level = NULL) {
  if (is.null(level)) {
    current <- logger::log_threshold(namespace = "castvote")
    return(names(log_level_numbers)[match(current, log_level_numbers)])
  }

  level <- toupper(as.character(level))
  if (
    length(level) != 1L || is.na(level) || !level %in% names(log_level_numbers)
  ) {
    cli::cli_abort(
      "{.arg level} must be one of {.val {names(log_level_numbers)}}.",
      class = "castvote_error_log_level"
    )
  }

  logger::log_threshold(level, namespace = "castvote")
  invisible(level)
}

#' @noRd
cv_log_info <- function(...) {
  logger::log_info(..., namespace = "castvote")
}

#' @noRd
cv_log_debug <- function(...) {
  logger::log_debug(..., namespace = "castvote")
}

#' @noRd
cv_log_warn <- function(...) {
  logger::log_warn(..., namespace = "castvote")
}

#' @noRd
cv_log_error <- function(...) {
  logger::log_error(..., namespace = "castvote")
}

.onLoad <- function(libname, pkgname) {
  level <- getOption(
    "castvote.log_level",
    Sys.getenv("CASTVOTE_LOG_LEVEL", "WARN")
  )
  try(castvote_log_level(level), silent = TRUE)
  invisible()
}
