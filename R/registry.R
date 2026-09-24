#' @noRd
castvote_registry <- function() {
  path <- system.file("extdata", "elections.csv", package = "castvote")
  data.table::fread(path, na.strings = c("", "NA"))
}

#' Registered elections
#'
#' The packaged registry of elections and their source files.
#'
#' @return A data table with one row per election: `id`, `date`, `jurisdiction`,
#'   `office`, `seats`, `vendor`, and `format`.
#' @export
castvote_elections <- function() {
  registry <- castvote_registry()
  unique(registry[, list(id, date, jurisdiction, office, seats, vendor, format)])
}

#' Source files for an election
#'
#' @param id An election id from [castvote_elections()].
#' @return A data table with one row per file: `local_name`, `url`, `sha256`,
#'   `bytes`, and `role`.
#' @export
castvote_election_files <- function(id) {
  checkmate::assert_string(id)
  target <- id
  registry <- castvote_registry()
  out <- registry[registry[["id"]] == target]
  if (!nrow(out)) {
    cli::cli_abort(
      "Unknown election {.val {id}}. See {.fun castvote_elections}.",
      class = "castvote_error_registry"
    )
  }
  out[, list(id, role, local_name, url, sha256, bytes, format)]
}
