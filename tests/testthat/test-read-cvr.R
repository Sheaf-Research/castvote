test_that("read_cvr() requires a lookup for WinEDS input", {
  fixture <- wineds_fixture()

  expect_error(
    read_cvr(fixture$ballot, format = "wineds"),
    class = "castvote_error_dispatch"
  )
})

test_that("read_cvr() dispatches to the WinEDS reader", {
  fixture <- wineds_fixture()

  marks <- read_cvr(
    fixture$ballot,
    format = "wineds",
    lookup = fixture$lookup
  )

  expect_s3_class(marks, "cvr_marks")
  expect_identical(nrow(marks), 5L)
})

test_that("read_cvr() infers the Dominion format from a zip path", {
  expect_identical(castvote_format("/tmp/cvr.zip"), "dominion")
  expect_identical(castvote_format("/tmp/20180627_ballotimage.txt"), "wineds")
})
