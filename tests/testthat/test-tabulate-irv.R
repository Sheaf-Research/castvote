three_candidate_contest <- function() {
  rankings <- c(
    repeat_rankings(c("A", "B", "C"), 4, "a"),
    repeat_rankings(c("B", "A", "C"), 3, "b"),
    repeat_rankings(c("C", "B", "A"), 2, "c")
  )
  marks_from_rankings(rankings)
}

test_that("a first-round majority wins without transfers", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 6, "a"),
    repeat_rankings(c("B", "A"), 4, "b")
  )

  result <- tabulate_irv(marks_from_rankings(rankings), sf_2018_rules())

  expect_identical(result$winner, "A")
  expect_identical(nrow(result$rounds), 2L)
  expect_identical(result$eliminated, character())
})

test_that("the lowest candidate is eliminated and transfers decide the winner", {
  result <- tabulate_irv(three_candidate_contest(), sf_2018_rules())

  expect_identical(result$winner, "B")
  expect_identical(result$eliminated, "C")
  final <- result$rounds[round == max(round)]
  expect_identical(final[candidate_id == "B", votes], 5L)
})

test_that("an overvote exhausts the ballot at that rank", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 4, "b"),
    repeat_rankings(c("C", "A"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)
  marks[
    ballot_id == "c1" & rank == 1L,
    `:=`(
      is_overvote = TRUE,
      candidate_id = NA_character_
    )
  ]

  result <- tabulate_irv(marks, sf_2018_rules())

  expect_identical(result$winner, "A")
  expect_true(result$exhausted > 0L)
})

test_that("a skipped rank is passed over when skipped_rank is continue", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 4, "b"),
    repeat_rankings(c("C", "B"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)
  marks[
    ballot_id == "c1" & rank == 1L,
    `:=`(
      is_skipped = TRUE,
      candidate_id = NA_character_
    )
  ]

  result <- tabulate_irv(marks, sf_2018_rules())

  expect_identical(result$winner, "B")
})

test_that("a repeated candidate is ignored after its first ranking", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 3, "b"),
    repeat_rankings(c("C", "B", "C"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)

  result <- tabulate_irv(marks, sf_2018_rules())

  expect_identical(result$winner, "B")
})

test_that("invalidate_ballot discards a ballot with a repeated candidate", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 5, "a"),
    repeat_rankings(c("B", "A"), 4, "b"),
    repeat_rankings(c("C", "A", "C"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)
  rules <- irv_rules(
    year = 2018L,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_ballot",
    tie = "error"
  )

  result <- tabulate_irv(marks, rules)

  expect_identical(result$winner, "A")
})

test_that("an elimination tie errors unless a lot is configured", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 2, "b"),
    repeat_rankings(c("C", "A"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)

  expect_error(
    tabulate_irv(marks, sf_2018_rules()),
    class = "castvote_error_tie"
  )
})

test_that("a seeded lot resolves an elimination tie", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 2, "b"),
    repeat_rankings(c("C", "A"), 2, "c")
  )
  marks <- marks_from_rankings(rankings)
  rules <- irv_rules(
    year = 2018L,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_duplicate",
    tie = "lot"
  )

  result <- tabulate_irv(marks, rules, seed = 1L)

  expect_length(result$eliminated, 1L)
  expect_named(
    result$tie_breaks,
    c("round", "tied_candidates", "eliminated", "method")
  )
})

test_that("batch elimination removes every tied lowest candidate below the threshold", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 25, "a"),
    repeat_rankings(c("B", "A"), 20, "b"),
    repeat_rankings(c("C", "A"), 3, "c"),
    repeat_rankings(c("D", "A"), 3, "d")
  )
  marks <- marks_from_rankings(rankings)
  rules <- irv_rules(
    year = 2018L,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_duplicate",
    batch_elimination = TRUE,
    tie = "error"
  )

  result <- tabulate_irv(marks, rules)

  expect_true(all(c("C", "D") %in% result$eliminated))
})

test_that("ballot weights scale the tallies", {
  rankings <- c(
    repeat_rankings(c("A", "B"), 4, "a"),
    repeat_rankings(c("B", "A"), 4, "b"),
    repeat_rankings(c("C", "B"), 2, "c")
  )
  marks <- canonicalize_ballots(marks_from_rankings(rankings))
  weights <- stats::setNames(rep(2, 10), unique(marks$ballot_id))

  result <- tabulate_irv(marks, sf_2018_rules(), ballot_weights = weights)

  expect_identical(result$rounds[round == 1L & candidate_id == "A", votes], 8)
})
