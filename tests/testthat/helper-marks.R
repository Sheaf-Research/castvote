marks_fixture <- function() {
  data.table::data.table(
    ballot_id = c("b1", "b1", "b2", "b2"),
    contest_id = "mayor",
    precinct_id = "p1",
    rank = c(1L, 2L, 1L, 2L),
    candidate_id = c("ALICE", "BOB", "CAROL", NA_character_),
    is_overvote = c(FALSE, FALSE, FALSE, TRUE),
    is_skipped = c(FALSE, FALSE, FALSE, FALSE)
  )
}
