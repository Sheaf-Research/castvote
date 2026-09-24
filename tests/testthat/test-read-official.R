test_that("read_official_rounds() parses a San Francisco round report", {
  path <- Sys.getenv("CASTVOTE_2018_ASSESSOR_XLS")
  skip_if_not(
    nzchar(path) && file.exists(path),
    "set CASTVOTE_2018_ASSESSOR_XLS to run the official-rounds check"
  )

  official <- read_official_rounds(path)

  expect_identical(official$winner, "CARMEN CHU")
  expect_true("PAUL BELLAR" %in% official$rounds$candidate_id)
  final <- official$rounds[round == max(round) & votes > 0]
  expect_identical(final[candidate_id == "CARMEN CHU", votes], 236697L)
})
