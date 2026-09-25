#' Infer the cast-vote-record format from a path
#'
#' @param path A file path.
#' @return `"dominion"` for a zip or JSON export, otherwise `"wineds"`.
#' @noRd
castvote_format <- function(path) {
  if (grepl("\\.(zip|json)$", path, ignore.case = TRUE)) {
    "dominion"
  } else {
    "wineds"
  }
}

#' Read a cast vote record
#'
#' Dispatches to the reader for the detected or declared format.
#'
#' @param path A path to the cast vote record.
#' @param format One of `"auto"`, `"wineds"`, or `"dominion"`.
#' @param lookup The master lookup, required for WinEDS input.
#' @param ... Passed to the reader.
#' @return Canonical marks as a data table.
#' @export
read_cvr <- function(
  path,
  format = c("auto", "wineds", "dominion"),
  lookup = NULL,
  ...
) {
  format <- match.arg(format)
  if (identical(format, "auto")) {
    format <- castvote_format(path)
  }

  switch(
    format,
    wineds = {
      if (is.null(lookup)) {
        cli::cli_abort(
          "WinEDS input needs {.arg lookup}.",
          class = "castvote_error_dispatch"
        )
      }
      read_wineds(path, lookup, ...)
    },
    dominion = read_dominion(path, ...),
    cli::cli_abort(
      "Unsupported cast vote record format {.val {format}}.",
      class = "castvote_error_dispatch"
    )
  )
}
