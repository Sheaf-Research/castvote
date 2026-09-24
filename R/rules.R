#' Instant-runoff rule options
#'
#' @return A named list of accepted values for each configurable rule.
#' @noRd
irv_rule_options <- function() {
  list(
    overvote = c("exhaust_at_rank", "exhaust_ballot", "error"),
    skipped_rank = c("continue", "stop_at_gap", "exhaust_ballot"),
    duplicate_candidate = c(
      "invalidate_duplicate",
      "invalidate_ballot",
      "error"
    ),
    tie = c("error", "lot", "external_order"),
    majority = c("continuing_ballots", "total_ballots")
  )
}

#' Instant-runoff tabulation rules
#'
#' @param year Election year the rules describe.
#' @param rank_cap Maximum number of ranks a voter may mark.
#' @param overvote,skipped_rank,duplicate_candidate,tie How the tabulator treats
#'   an overvote, a skipped rank, a repeated candidate and an elimination tie.
#'   See [irv_rule_options()] for the accepted values.
#' @param batch_elimination Whether to eliminate every candidate whose combined
#'   total is below the next candidate's.
#' @param initial_majority,subsequent_majority Which ballot pool the majority
#'   threshold is measured against.
#' @return An object of class `irv_rules`.
#' @export
irv_rules <- function(
  year,
  rank_cap,
  overvote,
  skipped_rank,
  duplicate_candidate,
  batch_elimination = FALSE,
  tie,
  initial_majority = "continuing_ballots",
  subsequent_majority = "continuing_ballots"
) {
  options <- irv_rule_options()
  checkmate::assert_integerish(year, len = 1L, lower = 1L, any.missing = FALSE)
  checkmate::assert_integerish(
    rank_cap,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  checkmate::assert_choice(overvote, options$overvote)
  checkmate::assert_choice(skipped_rank, options$skipped_rank)
  checkmate::assert_choice(duplicate_candidate, options$duplicate_candidate)
  checkmate::assert_flag(batch_elimination)
  checkmate::assert_choice(tie, options$tie)
  checkmate::assert_choice(initial_majority, options$majority)
  checkmate::assert_choice(subsequent_majority, options$majority)

  structure(
    list(
      year = as.integer(year),
      rank_cap = as.integer(rank_cap),
      overvote = overvote,
      skipped_rank = skipped_rank,
      duplicate_candidate = duplicate_candidate,
      batch_elimination = batch_elimination,
      tie = tie,
      initial_majority = initial_majority,
      subsequent_majority = subsequent_majority
    ),
    class = "irv_rules"
  )
}

#' San Francisco tabulation rules
#'
#' @param year Election year. Only 2018 is defined.
#' @return An `irv_rules` object.
#' @export
sf_rules <- function(year) {
  year <- as.integer(year)
  if (!identical(year, 2018L)) {
    cli::cli_abort(
      "Unsupported San Francisco rules year: {.val {year}}.",
      class = "castvote_error_rules"
    )
  }
  irv_rules(
    year = year,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_duplicate",
    batch_elimination = TRUE,
    tie = "lot",
    initial_majority = "total_ballots",
    subsequent_majority = "continuing_ballots"
  )
}

#' San Francisco candidate universe
#'
#' @param year Election year. Only 2018 is defined.
#' @param contest Contest name. Only `"Mayor"` is defined.
#' @return A character vector of candidate labels.
#' @export
sf_candidate_ids <- function(year, contest = "Mayor") {
  checkmate::assert_integerish(year, len = 1L, any.missing = FALSE)
  checkmate::assert_string(contest)
  if (!identical(as.integer(year), 2018L) || !identical(contest, "Mayor")) {
    cli::cli_abort(
      "Unsupported San Francisco candidate universe.",
      class = "castvote_error_rules"
    )
  }
  c(
    "AMY FARAH WEISS",
    "ANGELA ALIOTO",
    "ELLEN LEE ZHOU",
    "JANE KIM",
    "LONDON BREED",
    "MARK LENO",
    "MICHELLE BRAVO",
    "RICHIE GREENBERG",
    "WRITE-IN ANTOINE R. ROGERS"
  )
}
