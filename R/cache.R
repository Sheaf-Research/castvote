#' Cache directory
#'
#' Where downloaded source files are stored. Override with the
#' `castvote.cache_dir` option.
#'
#' @return A path.
#' @export
castvote_cache_dir <- function() {
  getOption("castvote.cache_dir", tools::R_user_dir("castvote", "cache"))
}

#' Local data directory
#'
#' An optional directory of already-downloaded files, checked before the cache
#' and before any network request. Override with the `castvote.data_dir` option
#' or the `CASTVOTE_DATA_DIR` environment variable.
#'
#' @return A path, or `NA` when unset.
#' @export
castvote_data_dir <- function() {
  dir <- getOption(
    "castvote.data_dir",
    Sys.getenv("CASTVOTE_DATA_DIR", unset = NA_character_)
  )
  if (is.na(dir) || !nzchar(dir)) NA_character_ else dir
}

#' Verify a file checksum
#'
#' @param path A file path.
#' @param sha256 The expected SHA-256 digest.
#' @return The path, invisibly.
#' @export
castvote_verify_sha256 <- function(path, sha256) {
  checkmate::assert_file_exists(path, access = "r")
  checkmate::assert_string(sha256, min.chars = 1L)

  actual <- digest::digest(file = path, algo = "sha256")
  if (!identical(tolower(actual), tolower(sha256))) {
    cli::cli_abort(
      "Checksum mismatch for {.path {basename(path)}}: expected {sha256}, got {actual}.",
      class = "castvote_error_checksum"
    )
  }
  invisible(path)
}

#' @noRd
castvote_download_file <- function(row) {
  dest <- file.path(castvote_cache_dir(), row$local_name)
  if (!dir.exists(dirname(dest))) {
    dir.create(dirname(dest), recursive = TRUE)
  }
  cv_log_info("Downloading %s.", row$local_name)
  utils::download.file(row$url, dest, mode = "wb", quiet = TRUE)
  if (!is.na(row$sha256) && nzchar(row$sha256)) {
    castvote_verify_sha256(dest, row$sha256)
  }
  dest
}

#' @noRd
castvote_local_path <- function(row, download = FALSE) {
  candidates <- character()
  data_dir <- castvote_data_dir()
  if (!is.na(data_dir)) {
    candidates <- c(candidates, file.path(data_dir, row$local_name))
  }
  candidates <- c(candidates, file.path(castvote_cache_dir(), row$local_name))

  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  if (!isTRUE(download)) {
    return(NA_character_)
  }
  castvote_download_file(row)
}

#' Download an election's source files
#'
#' Downloads every file for an election into the cache and verifies its
#' checksum.
#'
#' @param id An election id from [castvote_elections()].
#' @param overwrite Whether to re-download files that already exist.
#' @return A named character vector of file paths, named by role.
#' @export
castvote_download <- function(id, overwrite = FALSE) {
  files <- castvote_election_files(id)
  paths <- vapply(
    seq_len(nrow(files)),
    function(i) {
      row <- files[i]
      dest <- file.path(castvote_cache_dir(), row$local_name)
      if (file.exists(dest) && !overwrite) {
        if (!is.na(row$sha256) && nzchar(row$sha256)) {
          castvote_verify_sha256(dest, row$sha256)
        }
        return(dest)
      }
      castvote_download_file(row)
    },
    character(1L)
  )
  stats::setNames(paths, files$role)
}

#' Fetch and read a registered election
#'
#' Resolves an election's files from the local data directory or cache
#' (downloading them when needed), reads them, and returns canonical marks.
#'
#' @param id An election id from [castvote_elections()].
#' @param contest Optional contest label to keep.
#' @param download Whether to download missing files.
#' @param ... Passed to the reader.
#' @return Canonical marks as a data table.
#' @export
castvote_fetch <- function(id, contest = NULL, download = TRUE, ...) {
  files <- castvote_election_files(id)
  format <- files$format[1L]
  if (!format %in% c("wineds", "dominion")) {
    cli::cli_abort(
      "Election {.val {id}} has format {.val {format}}; read it with {.fun read_official_rounds}.",
      class = "castvote_error_registry"
    )
  }

  paths <- lapply(seq_len(nrow(files)), function(i) {
    castvote_local_path(files[i], download = download)
  })
  names(paths) <- files$role
  missing <- names(paths)[vapply(paths, is.na, logical(1L))]
  if (length(missing)) {
    cli::cli_abort(
      "Missing file{?s} for {.val {id}}: {.field {missing}}. Download them or set {.envvar CASTVOTE_DATA_DIR}.",
      class = "castvote_error_registry"
    )
  }

  marks <- if (identical(format, "wineds")) {
    read_wineds(paths[["ballot"]], paths[["lookup"]], ...)
  } else {
    read_dominion(paths[["cvr"]], ...)
  }

  if (!is.null(contest)) {
    marks <- marks[marks$contest_id == contest]
  }
  marks
}
