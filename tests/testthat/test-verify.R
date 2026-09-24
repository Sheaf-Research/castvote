official_three_candidate <- function() {
  list(
    winner = "B",
    rounds = data.table::data.table(
      round = c(1L, 1L, 1L, 2L, 2L),
      candidate_id = c("A", "B", "C", "A", "B"),
      votes = c(4L, 3L, 2L, 4L, 5L)
    ),
    eliminated = data.table::data.table(candidate_id = "C", votes = 2L),
    summaries = data.table::data.table(
      round = c(1L, 2L),
      total_ballots = c(9L, 9L)
    )
  )
}

test_that("verify_tabulator() reports a matching tabulation", {
  result <- tabulate_irv(three_candidate_contest(), sf_2018_rules())

  verification <- verify_tabulator(result, official_three_candidate())

  expect_true(verification$ok)
  expect_true(verification$winner)
  expect_true(verification$exhausted)
  expect_identical(nrow(verification$round_mismatches), 0L)
  expect_identical(nrow(verification$elimination_mismatches), 0L)
})

test_that("verify_tabulator() reports a vote mismatch", {
  official <- official_three_candidate()
  official$rounds[round == 2L & candidate_id == "B", votes := 4L]
  result <- tabulate_irv(three_candidate_contest(), sf_2018_rules())

  verification <- verify_tabulator(result, official)

  expect_false(verification$ok)
  expect_gt(nrow(verification$round_mismatches), 0L)
})
