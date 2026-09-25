#' Resolve one ballot against the current continuing candidate set
#' @noRd
resolve_ballot <- function(marks, continuing, rules) {
  candidate_marks <- marks[
    !is.na(candidate_id) &
      !is_invalid_writein &
      !is_overvote &
      !is_skipped
  ]
  first_ranks <- candidate_marks[,
    list(first_rank = min(rank)),
    by = candidate_id
  ]
  duplicate_ids <- first_ranks[
    candidate_marks[, .N, by = candidate_id][N > 1L],
    candidate_id,
    on = "candidate_id"
  ]

  if (length(duplicate_ids)) {
    if (identical(rules$duplicate_candidate, "invalidate_ballot")) {
      return(data.table::data.table(
        ballot_id = marks$ballot_id[1L],
        candidate_id = NA_character_,
        status = "exhausted_duplicate"
      ))
    }
    if (identical(rules$duplicate_candidate, "error")) {
      cli::cli_abort(
        "Duplicate candidate ranking on ballot {.val {marks$ballot_id[1L]}}.",
        class = "castvote_error_duplicate"
      )
    }
  }

  for (current_rank in sort(unique(marks$rank))) {
    rank_marks <- marks[marks[["rank"]] == current_rank]
    if (any(rank_marks$is_skipped)) {
      if (!identical(rules$skipped_rank, "continue")) {
        return(data.table::data.table(
          ballot_id = marks$ballot_id[1L],
          candidate_id = NA_character_,
          status = "exhausted_skipped_rank"
        ))
      }
      next
    }
    if (any(rank_marks$is_overvote)) {
      if (identical(rules$overvote, "error")) {
        cli::cli_abort(
          "Overvote on ballot {.val {marks$ballot_id[1L]}}.",
          class = "castvote_error_overvote"
        )
      }
      return(data.table::data.table(
        ballot_id = marks$ballot_id[1L],
        candidate_id = NA_character_,
        status = "exhausted_overvote"
      ))
    }
    candidate_rows <-
      !is.na(rank_marks$candidate_id) &
      !rank_marks$is_invalid_writein &
      !rank_marks$is_overvote &
      rank_marks$candidate_id %in% continuing
    if (identical(rules$duplicate_candidate, "invalidate_duplicate")) {
      first_rank_map <- stats::setNames(
        first_ranks$first_rank,
        first_ranks$candidate_id
      )
      candidate_rows <- candidate_rows &
        current_rank == unname(first_rank_map[rank_marks$candidate_id])
    }
    candidates <- rank_marks[candidate_rows, candidate_id]
    if (length(candidates)) {
      return(data.table::data.table(
        ballot_id = marks$ballot_id[1L],
        candidate_id = candidates[1L],
        status = "active"
      ))
    }
  }

  data.table::data.table(
    ballot_id = marks$ballot_id[1L],
    candidate_id = NA_character_,
    status = "exhausted"
  )
}

#' Resolve ballots against a continuing candidate set
#'
#' Selects each ballot's highest-ranked continuing candidate, applying the
#' overvote, skipped-rank and duplicate rules.
#'
#' @param ballots Cast-vote marks, in the [castvote_schema()] format.
#' @param continuing Candidate ids still in the running.
#' @param rules An [irv_rules()] object.
#' @return A data table with `ballot_id`, `candidate_id`, and `status`.
#' @export
resolve_ballots <- function(ballots, continuing, rules) {
  if (
    identical(rules$skipped_rank, "continue") &&
      identical(rules$overvote, "exhaust_at_rank")
  ) {
    return(resolve_prepared_ballots(
      prepare_ballots(ballots, rules),
      continuing
    ))
  }

  if (
    !identical(rules$skipped_rank, "continue") ||
      identical(rules$overvote, "error")
  ) {
    ballot_ids <- unique(ballots$ballot_id)
    return(data.table::rbindlist(lapply(ballot_ids, function(current_id) {
      resolve_ballot(
        ballots[ballot_id == current_id],
        continuing = continuing,
        rules = rules
      )
    })))
  }

  ballot_ids <- unique(ballots[, list(ballot_id)])
  candidate_marks <- ballots[
    !is.na(candidate_id) &
      !is_invalid_writein &
      !is_overvote &
      !is_skipped
  ]
  duplicate_ids <- candidate_marks[,
    .N,
    by = list(ballot_id, candidate_id)
  ][N > 1L, unique(ballot_id)]
  if (identical(rules$duplicate_candidate, "error") && length(duplicate_ids)) {
    cli::cli_abort(
      "Duplicate candidate ranking on ballot {.val {duplicate_ids[1L]}}.",
      class = "castvote_error_duplicate"
    )
  }

  invalid_duplicates <- data.table::data.table(
    ballot_id = character(),
    candidate_id = character(),
    status = character()
  )
  if (identical(rules$duplicate_candidate, "invalidate_ballot")) {
    invalid_duplicates <- data.table::data.table(
      ballot_id = duplicate_ids,
      candidate_id = NA_character_,
      status = "exhausted_duplicate"
    )
    candidate_marks <- candidate_marks[!ballot_id %in% duplicate_ids]
  } else if (identical(rules$duplicate_candidate, "invalidate_duplicate")) {
    first_ranks <- candidate_marks[,
      list(first_rank = min(rank)),
      by = list(ballot_id, candidate_id)
    ]
    candidate_marks <- first_ranks[
      candidate_marks,
      on = list(ballot_id, candidate_id),
      nomatch = 0L
    ][rank == first_rank, list(ballot_id, rank, candidate_id)]
  }

  candidate_marks <- candidate_marks[candidate_id %in% continuing]
  candidate_by_rank <- candidate_marks[
    order(ballot_id, rank, candidate_id),
    list(candidate_id = candidate_id[1L]),
    by = list(ballot_id, rank)
  ]
  rank_state <- ballots[,
    list(skipped = any(is_skipped), overvote = any(is_overvote)),
    by = list(ballot_id, rank)
  ][skipped == FALSE]
  rank_state <- rank_state[
    candidate_by_rank,
    on = list(ballot_id, rank),
    candidate_id := i.candidate_id
  ]
  rank_state <- rank_state[overvote | !is.na(candidate_id)]
  choices <- rank_state[
    order(ballot_id, rank),
    .SD[1L],
    by = ballot_id
  ][, `:=`(
    candidate_id = data.table::fifelse(
      overvote,
      NA_character_,
      candidate_id
    ),
    status = data.table::fifelse(overvote, "exhausted_overvote", "active")
  )][, list(ballot_id, candidate_id, status)]
  resolved_ids <- choices$ballot_id
  exhausted <- ballot_ids[
    !ballot_id %in%
      c(
        resolved_ids,
        invalid_duplicates$ballot_id
      )
  ][, list(
    ballot_id,
    candidate_id = NA_character_,
    status = "exhausted"
  )]

  data.table::rbindlist(
    list(invalid_duplicates, choices, exhausted),
    use.names = TRUE
  )[order(match(ballot_id, ballot_ids$ballot_id))]
}

#' Prepare ballot matrices for the fast resolution path
#' @noRd
prepare_ballots <- function(ballots, rules) {
  ballot_ids <- unique(ballots[, list(ballot_id)])
  ballot_index <- stats::setNames(
    seq_len(nrow(ballot_ids)),
    ballot_ids$ballot_id
  )
  candidate_marks <- ballots[
    !is.na(candidate_id) &
      !is_invalid_writein &
      !is_overvote &
      !is_skipped
  ]
  duplicate_ids <- candidate_marks[,
    .N,
    by = list(ballot_id, candidate_id)
  ][N > 1L, unique(ballot_id)]
  if (identical(rules$duplicate_candidate, "error") && length(duplicate_ids)) {
    cli::cli_abort(
      "Duplicate candidate ranking on ballot {.val {duplicate_ids[1L]}}.",
      class = "castvote_error_duplicate"
    )
  }

  invalid_duplicates <- data.table::data.table(
    ballot_id = duplicate_ids,
    candidate_id = NA_character_,
    status = "exhausted_duplicate"
  )
  if (identical(rules$duplicate_candidate, "invalidate_ballot")) {
    candidate_marks <- candidate_marks[!ballot_id %in% duplicate_ids]
  } else if (identical(rules$duplicate_candidate, "invalidate_duplicate")) {
    first_ranks <- candidate_marks[,
      list(first_rank = min(rank)),
      by = list(ballot_id, candidate_id)
    ]
    candidate_marks <- first_ranks[
      candidate_marks,
      on = list(ballot_id, candidate_id),
      nomatch = 0L
    ][rank == first_rank, list(ballot_id, rank, candidate_id)]
    invalid_duplicates <- invalid_duplicates[0L]
  }

  max_rank <- max(ballots$rank)
  candidate_matrix <- matrix(
    NA_character_,
    nrow = nrow(ballot_ids),
    ncol = max_rank
  )
  candidate_marks <- candidate_marks[
    order(ballot_id, rank, candidate_id)
  ][!duplicated(candidate_marks[, list(ballot_id, rank)])]
  candidate_matrix[cbind(
    ballot_index[candidate_marks$ballot_id],
    candidate_marks$rank
  )] <- candidate_marks$candidate_id

  overvote_matrix <- matrix(
    FALSE,
    nrow = nrow(ballot_ids),
    ncol = max_rank
  )
  overvote_marks <- ballots[is_overvote == TRUE]
  if (nrow(overvote_marks)) {
    overvote_matrix[cbind(
      ballot_index[overvote_marks$ballot_id],
      overvote_marks$rank
    )] <- TRUE
  }

  list(
    ballot_ids = ballot_ids,
    candidate_matrix = candidate_matrix,
    overvote_matrix = overvote_matrix,
    invalid_duplicates = invalid_duplicates
  )
}

#' @noRd
resolve_prepared_ballots <- function(prepared, continuing) {
  n_ballots <- nrow(prepared$ballot_ids)
  choice <- rep(NA_character_, n_ballots)
  status <- rep("exhausted", n_ballots)
  unresolved <- rep(TRUE, n_ballots)
  for (rank in seq_len(ncol(prepared$candidate_matrix))) {
    overvote <- unresolved & prepared$overvote_matrix[, rank]
    status[overvote] <- "exhausted_overvote"
    unresolved[overvote] <- FALSE
    available <- unresolved &
      prepared$candidate_matrix[, rank] %in% continuing
    choice[available] <- prepared$candidate_matrix[available, rank]
    status[available] <- "active"
    unresolved[available] <- FALSE
  }

  invalid <- match(
    prepared$invalid_duplicates$ballot_id,
    prepared$ballot_ids$ballot_id
  )
  if (length(invalid)) {
    choice[invalid] <- NA_character_
    status[invalid] <- "exhausted_duplicate"
  }

  data.table::data.table(
    ballot_id = prepared$ballot_ids$ballot_id,
    candidate_id = choice,
    status = status
  )
}

#' Select each ballot's highest-ranked continuing candidate
#' @noRd
active_choices <- function(marks, continuing, rules = NULL, prepared = NULL) {
  if (is.null(rules)) {
    return(
      marks[
        !is.na(candidate_id) &
          !is_overvote &
          !is_skipped &
          candidate_id %in% continuing
      ][order(ballot_id, rank), .SD[1L], by = ballot_id]
    )
  }
  if (!is.null(prepared)) {
    return(resolve_prepared_ballots(prepared, continuing)[status == "active"])
  }
  resolve_ballots(marks, continuing, rules)[status == "active"]
}

#' Select the candidate set to eliminate from a round's counts
#' @noRd
elimination_set <- function(counts, rules = NULL, lot_draw = NULL) {
  lowest_votes <- min(counts$votes)
  lowest <- counts[votes == lowest_votes, candidate_id]
  if (length(lowest) == 1L) {
    return(lowest)
  }

  if (!any(counts$votes > lowest_votes)) {
    next_votes <- NA_integer_
  } else {
    next_votes <- min(counts[votes > lowest_votes, votes])
  }
  batch <- !is.null(rules) && isTRUE(rules$batch_elimination)
  if (
    batch && !is.na(next_votes) && length(lowest) * lowest_votes < next_votes
  ) {
    return(lowest)
  }

  tie <- if (is.null(rules)) "error" else rules$tie
  if (identical(tie, "lot")) {
    draw <- if (is.null(lot_draw)) {
      sample(lowest, size = 1L)
    } else {
      lot_draw(lowest)
    }
    checkmate::assert_choice(draw, lowest)
    return(draw)
  }
  if (identical(tie, "external_order")) {
    cli::cli_abort(
      "Elimination tie requires an external candidate order.",
      class = "castvote_error_tie"
    )
  }
  cli::cli_abort(
    "Elimination tie in the current round.",
    class = "castvote_error_tie"
  )
}
