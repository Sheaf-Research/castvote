#' Verify a tabulation against an official report
#'
#' Reconstructs the round states from an official round-by-round report and
#' compares them with a [tabulate_irv()] result: the winner, every round's
#' candidate totals, the elimination order, and the exhausted count.
#'
#' @param tabulation A [tabulate_irv()] result.
#' @param official A list with `winner`, `rounds` (`candidate_id`, `votes`,
#'   `round`), `eliminated` (`candidate_id`, `votes`), and `summaries`
#'   (`round`, `total_ballots`).
#' @return A list with `ok`, the per-check flags, the mismatch tables, and the
#'   official exhausted count.
#' @export
verify_tabulator <- function(tabulation, official) {
  checkmate::assert_list(tabulation)
  checkmate::assert_list(official)
  checkmate::assert_names(
    names(tabulation),
    must.include = c("winner", "rounds", "eliminated", "exhausted")
  )
  checkmate::assert_names(
    names(official),
    must.include = c("winner", "rounds", "eliminated", "summaries")
  )

  expected_rounds <- official$rounds[
    votes > 0,
    list(candidate_id, votes, official_round = round)
  ]
  expected_rounds[,
    state := paste(sort(paste(candidate_id, votes)), collapse = "|"),
    by = official_round
  ]
  state_index <- unique(expected_rounds[, list(official_round, state)])
  state_index <- state_index[!duplicated(state)]
  state_index[, round := seq_len(.N)]
  expected_rounds[, round := match(state, state_index$state)]
  expected_rounds[, c("official_round", "state") := NULL]

  actual_rounds <- tabulation$rounds[, list(round, candidate_id, votes)]
  round_mismatches <- data.table::merge.data.table(
    actual_rounds,
    expected_rounds[, list(round, candidate_id, official_votes = votes)],
    by = c("round", "candidate_id"),
    all = TRUE
  )[
    is.na(votes) | is.na(official_votes) | votes != official_votes,
    list(round, candidate_id, votes, official_votes)
  ]

  actual_eliminated <- data.table::data.table(
    round = seq_along(tabulation$eliminated),
    candidate_id = tabulation$eliminated
  )
  expected_eliminated <- official$eliminated[
    votes > 0,
    list(round = seq_len(.N), candidate_id)
  ]
  elimination_mismatches <- data.table::merge.data.table(
    actual_eliminated,
    expected_eliminated,
    by = "round",
    all = TRUE,
    suffixes = c("", "_official")
  )[
    candidate_id != candidate_id_official |
      is.na(candidate_id) |
      is.na(candidate_id_official),
    list(round, candidate_id, candidate_id_official)
  ]

  final_state <- official$rounds[round == max(round) & votes > 0, sum(votes)]
  expected_exhausted <- official$summaries[
    round == max(round),
    total_ballots
  ] -
    final_state

  winner_matches <- identical(tabulation$winner, official$winner)
  exhausted_matches <- identical(tabulation$exhausted, expected_exhausted)
  ok <- winner_matches &&
    exhausted_matches &&
    nrow(round_mismatches) == 0L &&
    nrow(elimination_mismatches) == 0L

  cv_log_info("Verification %s.", if (ok) "passed" else "failed")

  list(
    ok = ok,
    winner = winner_matches,
    exhausted = exhausted_matches,
    round_mismatches = round_mismatches,
    elimination_mismatches = elimination_mismatches,
    official_exhausted = expected_exhausted
  )
}
