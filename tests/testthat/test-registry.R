test_that("castvote_elections() lists the registered elections", {
  elections <- castvote_elections()

  expect_s3_class(elections, "data.table")
  expect_true("sf-2019-11-mayor" %in% elections$id)
  expect_true(all(
    c("id", "date", "jurisdiction", "office", "vendor", "format") %in%
      names(elections)
  ))
})

test_that("castvote_election_files() returns the file rows for an election", {
  files <- castvote_election_files("sf-2019-11-mayor")

  expect_true(any(grepl("CVR_Export", files$local_name)))
  expect_identical(nrow(files), 1L)
  expect_true(all(
    c("local_name", "url", "sha256", "role") %in% names(files)
  ))
})

test_that("castvote_election_files() rejects an unknown election", {
  expect_error(
    castvote_election_files("does-not-exist"),
    class = "castvote_error_registry"
  )
})

test_that("castvote_cache_dir() honors an explicit option", {
  path <- file.path(tempdir(), "castvote-cache")
  withr::local_options(castvote.cache_dir = path)

  expect_identical(castvote_cache_dir(), path)
})

test_that("castvote_data_dir() honors the environment variable", {
  path <- file.path(tempdir(), "castvote-data")
  withr::local_envvar(CASTVOTE_DATA_DIR = path)

  expect_identical(castvote_data_dir(), path)
})

test_that("castvote_verify_sha256() checks a digest", {
  path <- tempfile()
  writeLines("castvote", path)
  good <- digest::digest(file = path, algo = "sha256")

  expect_invisible(castvote_verify_sha256(path, good))
  expect_error(
    castvote_verify_sha256(path, strrep("0", 64L)),
    class = "castvote_error_checksum"
  )
})

test_that("castvote_fetch() reads registered files from a local data directory", {
  data_dir <- Sys.getenv("CASTVOTE_DATA_DIR")
  skip_if_not(
    nzchar(data_dir) && dir.exists(data_dir),
    "set CASTVOTE_DATA_DIR to run the registry fetch check"
  )

  files <- castvote_election_files("sf-2018-06-mayor")
  ballot <- files[files$role == "ballot", ]
  expect_invisible(castvote_verify_sha256(
    file.path(data_dir, ballot$local_name),
    ballot$sha256
  ))

  marks <- castvote_fetch("sf-2018-06-mayor", contest = "Mayor")

  expect_s3_class(marks, "data.table")
  expect_identical(data.table::uniqueN(marks$ballot_id), 254016L)
})
