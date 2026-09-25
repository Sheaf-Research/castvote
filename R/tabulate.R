#' Validate a candidate universe
#'
#' @param candidate_ids Character vector of candidate ids.
#' @return The ids, unchanged.
#' @export
validate_candidate_ids <- function(candidate_ids) {
  checkmate::assert_character(
    candidate_ids,
    min.len = 1L,
    min.chars = 1L,
    any.missing = FALSE
  )
  if (anyDuplicated(candidate_ids)) {
    cli::cli_abort(
      "{.arg candidate_ids} must be unique.",
      class = "castvote_error_candidates"
    )
  }
  candidate_ids
}

#' Tabulate an instant-runoff election
#'
#' Runs the full elimination sequence over canonical marks and returns the
#' winner, every round's tally, the elimination order, and the exhausted count.
#'
#' @param marks Cast-vote marks, in the [castvote_schema()] format.
#' @param rules An [irv_rules()] object, or `NULL` for the default behavior.
#' @param seed Optional integer seed for tie-breaking lots.
#' @param candidate_ids Optional candidate universe. Defaults to the candidates
#'   observed in the marks.
#' @param ballot_weights Optional positive weights named by ballot.
#' @param marks_are_canonical Whether `marks` is already canonical, skipping the
#'   validation pass.
#' @return A list with `winner`, `rounds`, `eliminated`, `exhausted`, `rules`,
#'   and `tie_breaks`.
#' @export
tabulate_irv <- function(
  marks,
  rules = NULL,
  seed = NULL,
  candidate_ids = NULL,
  ballot_weights = NULL,
  marks_are_canonical = FALSE
) {
  if (!is.null(seed)) {
    checkmate::assert_integerish(seed, len = 1L, any.missing = FALSE)
    withr::local_seed(seed)
  }

  marks <- if (isTRUE(marks_are_canonical)) {
    data.table::as.data.table(marks)
  } else {
    canonicalize_ballots(marks)
  }

  if (!is.null(ballot_weights)) {
    checkmate::assert_numeric(
      ballot_weights,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_names(
      names(ballot_weights),
      type = "unique",
      permutation.of = unique(marks$ballot_id)
    )
    checkmate::assert_true(all(ballot_weights > 0))
  }

  observed_candidate_ids <- sort(unique(marks[
    !is.na(candidate_id) &
      !is_overvote &
      !is_skipped &
      !is_invalid_writein,
    candidate_id
  ]))
  if (!length(observed_candidate_ids)) {
    cli::cli_abort(
      "The marks contain no valid candidate rankings.",
      class = "castvote_error_candidates"
    )
  }
  if (is.null(candidate_ids)) {
    candidate_ids <- observed_candidate_ids
  } else {
    candidate_ids <- validate_candidate_ids(candidate_ids)
    outside_universe <- setdiff(observed_candidate_ids, candidate_ids)
    if (length(outside_universe)) {
      cli::cli_abort(
        "The marks contain candidates outside {.arg candidate_ids}: {.val {outside_universe}}.",
        class = "castvote_error_candidates"
      )
    }
  }

  prepared <- if (
    !is.null(rules) &&
      identical(rules$skipped_rank, "continue") &&
      identical(rules$overvote, "exhaust_at_rank")
  ) {
    prepare_ballots(marks, rules)
  } else {
    NULL
  }

  total_ballots <- data.table::uniqueN(marks$ballot_id)
  if (!is.null(ballot_weights)) {
    total_ballots <- sum(ballot_weights)
  }

  cv_log_info(
    "Tabulating %s ballots across %s candidates.",
    total_ballots,
    length(candidate_ids)
  )

  continuing <- candidate_ids
  eliminated <- character()
  tie_breaks <- list()
  rounds <- list()
  round_number <- 1L

  repeat {
    active <- active_choices(marks, continuing, rules, prepared)

    tallies <- if (is.null(ballot_weights)) {
      active[, list(votes = .N), by = candidate_id]
    } else {
      active[, list(votes = sum(ballot_weights[ballot_id])), by = candidate_id]
    }
    counts <- tallies[
      data.table::data.table(candidate_id = continuing),
      on = "candidate_id"
    ]
    counts[is.na(votes), votes := 0L]
    counts[, `:=`(round = round_number, continuing = TRUE)]
    counts <- counts[, list(round, candidate_id, votes, continuing)]
    rounds[[round_number]] <- counts

    active_votes <- sum(counts$votes)
    majority_mode <- if (!is.null(rules) && round_number == 1L) {
      rules$initial_majority
    } else if (!is.null(rules)) {
      rules$subsequent_majority
    } else {
      "continuing_ballots"
    }
    majority_votes <- if (
      !is.null(rules) && identical(majority_mode, "total_ballots")
    ) {
      total_ballots / 2
    } else {
      active_votes / 2
    }
    winner <- counts[votes > majority_votes, candidate_id]
    if (length(winner)) {
      break
    }
    if (length(continuing) == 1L) {
      winner <- continuing
      break
    }

    tied_lowest <- counts[votes == min(votes), candidate_id]
    lowest <- elimination_set(counts, rules)
    if (length(tied_lowest) > 1L && length(lowest) == 1L) {
      tie_breaks[[length(tie_breaks) + 1L]] <- data.table::data.table(
        round = round_number,
        tied_candidates = paste(tied_lowest, collapse = "|"),
        eliminated = lowest,
        method = if (!is.null(rules) && identical(rules$tie, "lot")) {
          "lot"
        } else {
          "external_order"
        }
      )
    }
    eliminated <- c(eliminated, lowest)
    continuing <- setdiff(continuing, lowest)
    cv_log_debug(
      "Round %s: eliminated %s.",
      round_number,
      paste(lowest, collapse = ", ")
    )
    round_number <- round_number + 1L
  }

  cv_log_info(
    "Winner %s with %s of %s ballots exhausted.",
    winner,
    total_ballots - active_votes,
    total_ballots
  )

  list(
    winner = winner,
    rounds = data.table::rbindlist(rounds),
    eliminated = eliminated,
    exhausted = total_ballots - active_votes,
    rules = rules,
    tie_breaks = data.table::rbindlist(
      tie_breaks,
      use.names = TRUE,
      fill = TRUE
    )
  )
}
