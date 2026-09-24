#' Read an official round-by-round report
#'
#' Reads a San Francisco official results workbook into the structures
#' [verify_tabulator()] expects: per-round candidate totals, the elimination
#' order, the transfers, and the contest summaries.
#'
#' @param path Path to an `.xls` or `.xlsx` round report.
#' @return A list with `rounds`, `eliminated`, `summaries`, `transfers`,
#'   `winner`, and `metadata`.
#' @export
read_official_rounds <- function(path) {
  checkmate::assert_file_exists(path)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort(
      "The readxl package is required to read official round reports.",
      class = "castvote_error_dependency"
    )
  }

  cells_for <- function(sheet) {
    cells <- as.matrix(readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      .name_repair = "minimal"
    ))
    cells <- apply(cells, 2L, as.character)
    cells[is.na(cells)] <- ""
    matrix(trimws(cells), nrow = nrow(cells))
  }

  first_row <- function(cells, predicate) {
    rows <- which(apply(cells, 1L, predicate))
    if (length(rows)) rows[1L] else NA_integer_
  }

  header_columns <- function(cells, start_row) {
    if (is.na(start_row)) {
      return(NULL)
    }
    candidate_rows <- seq.int(start_row, nrow(cells))
    header_rows <- candidate_rows[vapply(
      candidate_rows,
      function(row) {
        row_values <- cells[row, ]
        any(row_values == "Candidate") && any(row_values == "Votes")
      },
      logical(1L)
    )]
    header <- if (length(header_rows)) header_rows[1L] else NA_integer_
    if (is.na(header)) {
      return(NULL)
    }
    list(
      row = header,
      candidate = which(cells[header, ] == "Candidate")[1L],
      votes = which(cells[header, ] == "Votes")[1L]
    )
  }

  number_after <- function(cells, label) {
    position <- which(cells == label, arr.ind = TRUE)[1L, ]
    if (anyNA(position)) {
      return(NA_integer_)
    }
    values <- cells[position[1L], seq.int(position[2L] + 1L, ncol(cells))]
    values <- suppressWarnings(as.integer(gsub(",", "", values)))
    values[!is.na(values)][1L]
  }

  parse_candidate_state <- function(cells, section) {
    section_row <- first_row(cells, function(row) any(row == section))
    columns <- header_columns(cells, section_row)
    if (is.na(section_row) || is.null(columns)) {
      return(NULL)
    }

    rows <- seq.int(columns$row + 1L, nrow(cells))
    summary_labels <- c(
      "Continuing Ballots",
      "Exhausted by Over Votes",
      "Under Votes",
      "Exhausted Ballots",
      "Total Ballots"
    )
    names <- cells[rows, columns$candidate]
    stop_row <- which(names %in% summary_labels)[1L]
    if (!is.na(stop_row)) {
      rows <- rows[seq_len(stop_row - 1L)]
    }
    rows <- rows[nzchar(cells[rows, columns$candidate])]
    if (!length(rows)) {
      return(NULL)
    }

    data.table::data.table(
      candidate_id = cells[rows, columns$candidate],
      votes = as.integer(gsub(",", "", cells[rows, columns$votes])),
      eliminated = vapply(
        rows,
        function(row) {
          any(grepl("Eliminated in pass", cells[row, ], fixed = TRUE))
        },
        logical(1L)
      ),
      winner = vapply(
        rows,
        function(row) any(grepl("** WINNER **", cells[row, ], fixed = TRUE)),
        logical(1L)
      )
    )
  }

  parse_eliminated <- function(cells) {
    section_row <- first_row(cells, function(row) {
      any(grepl("Eliminated Candidates - Pass", row, fixed = TRUE))
    })
    columns <- header_columns(cells, section_row)
    if (is.na(section_row) || is.null(columns)) {
      return(NULL)
    }
    rows <- seq.int(columns$row + 1L, nrow(cells))
    stop_row <- which(vapply(
      rows,
      function(row) {
        any(cells[row, ] %in% c("Vote Changes - Pass", "Final State"))
      },
      logical(1L)
    ))[1L]
    if (!is.na(stop_row)) {
      rows <- rows[seq_len(stop_row - 1L)]
    }
    rows <- rows[nzchar(cells[rows, columns$candidate])]
    data.table::data.table(
      candidate_id = cells[rows, columns$candidate],
      votes = as.integer(gsub(",", "", cells[rows, columns$votes]))
    )
  }

  parse_transfers <- function(cells) {
    section_row <- first_row(cells, function(row) {
      any(grepl("Vote Changes - Pass", row, fixed = TRUE))
    })
    if (is.na(section_row)) {
      return(NULL)
    }
    header <- first_row(cells, function(row) {
      any(row == "From") &&
        any(row == "To") &&
        any(row == "Exhausted") &&
        any(row == "Transferred")
    })
    if (is.na(header)) {
      return(NULL)
    }
    columns <- vapply(
      c("From", "To", "Exhausted", "Transferred"),
      function(label) which(cells[header, ] == label)[1L],
      integer(1L)
    )
    rows <- seq.int(header + 1L, nrow(cells))
    stop_row <- which(vapply(
      rows,
      function(row) any(cells[row, ] == "Total"),
      logical(1L)
    ))[1L]
    if (!is.na(stop_row)) {
      rows <- rows[seq_len(stop_row - 1L)]
    }
    rows <- rows[nzchar(cells[rows, columns[["From"]]])]
    data.table::data.table(
      from_candidate = cells[rows, columns[["From"]]],
      to_candidate = cells[rows, columns[["To"]]],
      exhausted = as.integer(gsub(
        ",",
        "",
        cells[rows, columns[["Exhausted"]]]
      )),
      transferred = as.integer(gsub(
        ",",
        "",
        cells[rows, columns[["Transferred"]]]
      ))
    )
  }

  parse_sheet <- function(sheet) {
    cells <- cells_for(sheet)
    pass_cell <- cells[grepl("Pass Number:", cells, fixed = TRUE)][1L]
    if (is.na(pass_cell)) {
      return(NULL)
    }
    pass <- as.integer(sub(
      ".*Pass Number:[[:space:]]*([0-9]+).*",
      "\\1",
      pass_cell
    ))
    contest_position <- which(cells == "Contest:", arr.ind = TRUE)[1L, ]
    if (anyNA(contest_position)) {
      return(NULL)
    }
    contest_values <- cells[
      contest_position[1L],
      seq.int(contest_position[2L] + 1L, ncol(cells))
    ]
    contest_id <- contest_values[nzchar(contest_values)][1L]
    state <- parse_candidate_state(cells, "Final State")
    if (is.null(state)) {
      return(NULL)
    }

    state[, `:=`(
      contest_id = contest_id,
      round = pass + 1L,
      continuing_ballots = number_after(cells, "Continuing Ballots"),
      exhausted_overvotes = number_after(cells, "Exhausted by Over Votes"),
      under_votes = number_after(cells, "Under Votes"),
      exhausted_ballots = number_after(cells, "Exhausted Ballots"),
      total_ballots = number_after(cells, "Total Ballots")
    )]
    eliminated <- parse_eliminated(cells)
    if (!is.null(eliminated)) {
      eliminated[, `:=`(contest_id = contest_id, round = pass)]
    }
    transfers <- parse_transfers(cells)
    if (!is.null(transfers)) {
      transfers[, `:=`(contest_id = contest_id, round = pass)]
    }
    list(
      rounds = state[, .(
        contest_id,
        round,
        candidate_id,
        votes,
        eliminated,
        winner
      )],
      eliminated = eliminated,
      transfers = transfers,
      summaries = state[
        1L,
        .(
          contest_id,
          round,
          continuing_ballots,
          exhausted_overvotes,
          under_votes,
          exhausted_ballots,
          total_ballots
        )
      ]
    )
  }

  bind_or_empty <- function(items, columns) {
    items <- Filter(Negate(is.null), items)
    if (!length(items)) {
      empty <- data.table::data.table()
      empty[, (columns) := lapply(columns, function(x) vector("list", 0L))]
      return(empty)
    }
    data.table::rbindlist(items, use.names = TRUE, fill = TRUE)
  }

  parsed <- lapply(readxl::excel_sheets(path), parse_sheet)
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) {
    cli::cli_abort(
      "No official result sheets found in {.path {path}}.",
      class = "castvote_error_official"
    )
  }
  winner <- unique(unlist(lapply(parsed, function(x) {
    x$rounds$candidate_id[x$rounds$winner]
  })))
  winner <- winner[!is.na(winner)]

  cv_log_info(
    "Read %s round sheets from %s.",
    length(parsed),
    basename(path)
  )

  list(
    rounds = bind_or_empty(
      lapply(parsed, `[[`, "rounds"),
      c(
        "contest_id",
        "round",
        "candidate_id",
        "votes",
        "eliminated",
        "winner"
      )
    ),
    eliminated = bind_or_empty(
      lapply(parsed, `[[`, "eliminated"),
      c("candidate_id", "votes", "contest_id", "round")
    ),
    summaries = bind_or_empty(
      lapply(parsed, `[[`, "summaries"),
      c(
        "contest_id",
        "round",
        "continuing_ballots",
        "exhausted_overvotes",
        "under_votes",
        "exhausted_ballots",
        "total_ballots"
      )
    ),
    transfers = bind_or_empty(
      lapply(parsed, `[[`, "transfers"),
      c(
        "from_candidate",
        "to_candidate",
        "exhausted",
        "transferred",
        "contest_id",
        "round"
      )
    ),
    winner = if (length(winner)) winner[1L] else NA_character_,
    metadata = list(path = path)
  )
}
