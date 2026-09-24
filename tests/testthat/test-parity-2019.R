test_that("the 2019 San Francisco mayoral CVR reproduces the official winner", {
  zip_path <- Sys.getenv("CASTVOTE_2019_ZIP")
  skip_if_not(
    nzchar(zip_path) && file.exists(zip_path),
    "set CASTVOTE_2019_ZIP to run the 2019 parity check"
  )

  marks <- read_dominion(zip_path)
  rules <- irv_rules(
    year = 2019L,
    rank_cap = 3L,
    overvote = "exhaust_at_rank",
    skipped_rank = "continue",
    duplicate_candidate = "invalidate_duplicate",
    batch_elimination = TRUE,
    tie = "lot",
    initial_majority = "total_ballots",
    subsequent_majority = "continuing_ballots"
  )
  result <- tabulate_irv(marks[contest_id == "MAYOR"], rules, seed = 1L)

  expect_identical(result$winner, "LONDON N. BREED")
})
