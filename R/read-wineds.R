#' Read a WinEDS ballot image and master lookup
#'
#' WinEDS is the fixed-width format San Francisco used before 2019. The ballot
#' image holds one row per mark; the master lookup names the contests,
#' candidates, precincts and tally types. Both are read together and resolved
#' into the [castvote_schema()] long format.
#'
#' @param ballot A path to the ballot image, a character vector of lines, or a
#'   single-column data frame.
#' @param lookup The master lookup, in the same forms as `ballot`.
#' @param b_header,l_header Whether the ballot image and lookup carry a header
#'   line.
#' @return Canonical `cvr_marks`.
#' @export
read_wineds <- function(ballot, lookup, b_header = FALSE, l_header = FALSE) {
  ballot_fields <- wineds_ballot_fields(wineds_lines(ballot, b_header))
  lookup_fields <- wineds_lookup_fields(wineds_lines(lookup, l_header))

  marks <- wineds_resolve(ballot_fields, lookup_fields)

  cv_log_info(
    "Read %s marks across %s contests from WinEDS.",
    nrow(marks),
    data.table::uniqueN(marks$contest_id)
  )

  canonicalize_ballots(marks)
}

#' @noRd
wineds_lines <- function(x, header) {
  lines <- if (is.data.frame(x)) {
    as.character(x[[1L]])
  } else if (is.character(x) && length(x) == 1L && file.exists(x)) {
    readLines(x, warn = FALSE)
  } else {
    as.character(x)
  }
  lines <- sub("\r$", "", lines)
  if (header && length(lines)) {
    lines <- lines[-1L]
  }
  lines
}

#' @noRd
wineds_ballot_fields <- function(lines) {
  data.table::data.table(
    contest_code = substr(lines, 1L, 7L),
    ballot_id = substr(lines, 8L, 16L),
    serial_number = substr(lines, 17L, 23L),
    tally_type_id = substr(lines, 24L, 26L),
    precinct_code = substr(lines, 27L, 33L),
    rank = substr(lines, 34L, 36L),
    candidate_code = substr(lines, 37L, 43L),
    is_overvote = substr(lines, 44L, 44L),
    is_skipped = substr(lines, 45L, 45L)
  )
}

#' @noRd
wineds_lookup_fields <- function(lines) {
  data.table::data.table(
    record_type = trimws(substr(lines, 1L, 10L)),
    id = substr(lines, 11L, 17L),
    description = trimws(substr(lines, 18L, 67L)),
    list_order = substr(lines, 68L, 74L),
    candidates_contest_id = substr(lines, 75L, 81L),
    is_writein = substr(lines, 82L, 82L),
    is_provisional = substr(lines, 83L, 83L)
  )
}

#' @noRd
wineds_flag <- function(x) {
  x <- trimws(x)
  x[!nzchar(x)] <- "0"
  x
}

#' @noRd
wineds_resolve <- function(ballot, lookup) {
  contests <- lookup[
    record_type == "Contest",
    list(contest_code = id, contest_id = description)
  ]
  candidates <- lookup[
    record_type == "Candidate",
    list(candidate_code = id, candidate_id = description)
  ]
  precincts <- lookup[
    record_type == "Precinct",
    list(precinct_code = id, precinct_id = description)
  ]
  tallies <- lookup[
    record_type == "Tally Type",
    list(tally_type_id = as.integer(id), tally = description)
  ]

  marks <- data.table::as.data.table(ballot)
  marks[, rank := as.integer(rank)]
  marks[, tally_type_id := as.integer(tally_type_id)]
  marks[, `:=`(is_overvote = wineds_flag(is_overvote))]
  marks[, `:=`(is_skipped = wineds_flag(is_skipped))]

  marks <- merge(
    marks,
    contests,
    by = "contest_code",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    candidates,
    by = "candidate_code",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    precincts,
    by = "precinct_code",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    tallies,
    by = "tally_type_id",
    all.x = TRUE,
    sort = FALSE
  )

  marks[, list(
    ballot_id,
    contest_id,
    precinct_id,
    rank,
    candidate_id,
    is_overvote,
    is_skipped,
    serial_number,
    tally
  )]
}
