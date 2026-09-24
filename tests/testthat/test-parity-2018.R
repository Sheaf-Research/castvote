test_that("the 2018 San Francisco mayoral CVR reproduces the official result", {
  ballot_path <- Sys.getenv("CASTVOTE_2018_BALLOT")
  lookup_path <- Sys.getenv("CASTVOTE_2018_LOOKUP")
  skip_if_not(
    nzchar(ballot_path) && file.exists(ballot_path),
    "set CASTVOTE_2018_BALLOT to run the 2018 parity check"
  )

  marks <- read_wineds(ballot_path, lookup_path)
  mayor <- marks[contest_id == "Mayor"]
  result <- tabulate_irv(mayor, sf_rules(2018))

  final_round <- result$rounds[round == max(round)]
  expect_identical(data.table::uniqueN(mayor$ballot_id), 254016L)
  expect_identical(result$winner, "LONDON BREED")
  expect_identical(
    final_round[candidate_id == "LONDON BREED", votes],
    115977L
  )
  expect_identical(final_round[candidate_id == "MARK LENO", votes], 113431L)
  expect_identical(result$exhausted, 24608L)
})
