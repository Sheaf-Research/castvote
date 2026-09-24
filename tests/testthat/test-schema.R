test_that("castvote_schema() declares the required mark columns", {
  schema <- castvote_schema()

  expect_s3_class(schema, "data.table")
  expect_named(schema, c("column", "type", "required"))
  expect_setequal(
    schema[required == TRUE, column],
    c(
      "ballot_id",
      "contest_id",
      "precinct_id",
      "rank",
      "candidate_id",
      "is_overvote",
      "is_skipped"
    )
  )
})

test_that("validate_ballots() rejects a missing required column", {
  x <- marks_fixture()
  x[, rank := NULL]

  expect_error(validate_ballots(x), class = "castvote_error_schema")
})

test_that("validate_ballots() rejects a missing ballot id", {
  x <- marks_fixture()
  x$ballot_id[1] <- NA_character_

  expect_error(validate_ballots(x), class = "castvote_error_marks")
})

test_that("validate_ballots() rejects a non-positive rank", {
  x <- marks_fixture()
  x$rank[1] <- 0L

  expect_error(validate_ballots(x), class = "castvote_error_marks")
})

test_that("validate_ballots() normalizes flags supplied as integers", {
  x <- marks_fixture()
  x[, is_overvote := as.integer(is_overvote)]

  validated <- validate_ballots(x)

  expect_type(validated$is_overvote, "logical")
})

test_that("validate_ballots() rejects a mark that is both overvote and skipped", {
  x <- marks_fixture()
  x$is_skipped[4] <- TRUE

  expect_error(validate_ballots(x), class = "castvote_error_marks")
})

test_that("validate_ballots() supplies a default write-in flag", {
  expect_false("is_invalid_writein" %in% names(marks_fixture()))

  validated <- validate_ballots(marks_fixture())

  expect_true(all(validated$is_invalid_writein == FALSE))
})

test_that("validate_ballots() does not mutate its input", {
  x <- marks_fixture()
  before <- data.table::copy(x)

  validate_ballots(x)

  expect_identical(x, before)
})

test_that("canonicalize_ballots() returns typed marks ordered by ballot and rank", {
  x <- marks_fixture()[c(3L, 1L, 4L, 2L)]
  x$rank <- as.numeric(x$rank)

  canonical <- canonicalize_ballots(x)

  expect_s3_class(canonical, "cvr_marks")
  expect_type(canonical$rank, "integer")
  expect_type(canonical$ballot_id, "character")
  expect_identical(canonical$ballot_id, c("b1", "b1", "b2", "b2"))
  expect_identical(canonical$rank, c(1L, 2L, 1L, 2L))
})
