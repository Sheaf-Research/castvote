test_that("the default log level is WARN", {
  expect_identical(castvote_log_level(), "WARN")
})

test_that("castvote_log_level() sets and reads the level case-insensitively", {
  old <- castvote_log_level()
  withr::defer(castvote_log_level(old))

  castvote_log_level("debug")

  expect_identical(castvote_log_level(), "DEBUG")
})

test_that("castvote_log_level() rejects an unknown level", {
  expect_error(
    castvote_log_level("verbose"),
    class = "castvote_error_log_level"
  )
})

test_that("informational logs are suppressed at WARN and shown at INFO", {
  old <- castvote_log_level()
  withr::defer(castvote_log_level(old))

  castvote_log_level("warn")
  quiet <- capture.output(cv_log_info("hidden"), type = "message")
  expect_length(quiet, 0L)

  castvote_log_level("info")
  loud <- capture.output(cv_log_info("visible"), type = "message")
  expect_length(loud, 1L)
  expect_match(loud, "visible")
})

test_that("log messages are formatted with sprintf placeholders", {
  old <- castvote_log_level()
  withr::defer(castvote_log_level(old))
  castvote_log_level("info")

  line <- capture.output(
    cv_log_info("Tabulating %s ballots.", 9L),
    type = "message"
  )

  expect_match(line, "Tabulating 9 ballots.", fixed = TRUE)
})
