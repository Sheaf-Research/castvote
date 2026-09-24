marks_from_rankings <- function(
  rankings,
  contest_id = "c1",
  precinct_id = "p1"
) {
  rows <- Map(
    function(id, candidates) {
      data.table::data.table(
        ballot_id = id,
        contest_id = contest_id,
        precinct_id = precinct_id,
        rank = seq_along(candidates),
        candidate_id = candidates,
        is_overvote = FALSE,
        is_skipped = FALSE
      )
    },
    names(rankings),
    rankings
  )
  data.table::rbindlist(rows)
}

repeat_rankings <- function(ranking, n, prefix) {
  stats::setNames(rep(list(ranking), n), paste0(prefix, seq_len(n)))
}

sf_2018_rules <- function() {
  irv_rules(
    year = 2018L,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_duplicate",
    batch_elimination = FALSE,
    tie = "error"
  )
}
