#' Canonical cast-vote-mark schema
#'
#' The long-format contract that every reader produces: one row per mark on a
#' ballot, with the contest, precinct, rank and candidate resolved to labels.
#'
#' @return A [data.table::data.table()] with one row per schema column and the
#'   fields `column`, `type`, and `required`.
#' @export
castvote_schema <- function() {
  data.table::data.table(
    column = c(
      "ballot_id",
      "contest_id",
      "precinct_id",
      "rank",
      "candidate_id",
      "is_overvote",
      "is_skipped",
      "is_invalid_writein"
    ),
    type = c(
      "character",
      "character",
      "character",
      "integer",
      "character",
      "logical",
      "logical",
      "logical"
    ),
    required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
}

#' @noRd
required_schema_columns <- function() {
  schema <- castvote_schema()
  schema[required == TRUE, column]
}

#' @noRd
normalize_flag <- function(x, column) {
  if (is.logical(x)) {
    out <- x
  } else if (is.numeric(x) && all(is.na(x) | x %in% c(0, 1))) {
    out <- as.logical(x)
  } else {
    values <- tolower(trimws(as.character(x)))
    out <- rep(NA, length(values))
    out[values %in% c("true", "t", "1")] <- TRUE
    out[values %in% c("false", "f", "0")] <- FALSE
  }
  if (anyNA(out)) {
    cli::cli_abort(
      "{.field {column}} must contain logical values.",
      class = "castvote_error_marks"
    )
  }
  out
}

#' Validate normalized mark data
#'
#' Checks the [castvote_schema()] contract: required columns present, ballot
#' identifiers usable, ranks positive whole numbers, and the overvote, skipped
#' and write-in flags logical. A missing write-in flag defaults to `FALSE`. The
#' input is not modified; the validated copy is returned.
#'
#' @param marks A data frame of marks.
#' @return The validated marks as a data table.
#' @export
validate_ballots <- function(marks) {
  if (!is.data.frame(marks)) {
    cli::cli_abort(
      "{.arg marks} must be a data frame.",
      class = "castvote_error_marks"
    )
  }

  marks <- data.table::as.data.table(data.table::copy(marks))

  missing <- setdiff(required_schema_columns(), names(marks))
  if (length(missing)) {
    cli::cli_abort(
      "Missing required column{?s}: {.field {missing}}.",
      class = "castvote_error_schema"
    )
  }

  if (anyNA(marks$ballot_id) || !all(nzchar(as.character(marks$ballot_id)))) {
    cli::cli_abort(
      "{.field ballot_id} must be non-missing and non-empty.",
      class = "castvote_error_marks"
    )
  }

  rank <- suppressWarnings(as.numeric(marks$rank))
  if (
    anyNA(rank) ||
      !all(is.finite(rank)) ||
      !all(rank == trunc(rank)) ||
      any(rank < 1)
  ) {
    cli::cli_abort(
      "{.field rank} must be positive whole numbers.",
      class = "castvote_error_marks"
    )
  }

  marks[, is_overvote := normalize_flag(is_overvote, "is_overvote")]
  marks[, is_skipped := normalize_flag(is_skipped, "is_skipped")]
  if ("is_invalid_writein" %in% names(marks)) {
    marks[,
      is_invalid_writein := normalize_flag(
        is_invalid_writein,
        "is_invalid_writein"
      )
    ]
  } else {
    marks[, is_invalid_writein := FALSE]
  }

  if (any(marks$is_overvote & marks$is_skipped)) {
    cli::cli_abort(
      "A mark cannot be both an overvote and skipped.",
      class = "castvote_error_marks"
    )
  }

  marks[]
}

#' Canonicalize mark data
#'
#' Validates marks, coerces every column to the [castvote_schema()] type, and
#' orders rows by ballot, rank and candidate.
#'
#' @param marks A data frame of marks.
#' @return The canonical marks as a data table.
#' @export
canonicalize_ballots <- function(marks) {
  marks <- validate_ballots(marks)

  marks[, `:=`(
    ballot_id = as.character(ballot_id),
    contest_id = as.character(contest_id),
    precinct_id = as.character(precinct_id),
    candidate_id = as.character(candidate_id),
    rank = as.integer(rank),
    is_invalid_writein = as.logical(is_invalid_writein)
  )]

  data.table::setorderv(marks, c("ballot_id", "rank", "candidate_id"))
  marks[]
}
