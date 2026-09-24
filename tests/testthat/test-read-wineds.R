wineds_ballot_line <- function(
  contest,
  voter,
  precinct,
  rank,
  candidate,
  serial = "0000001",
  tally = "001",
  over = "0",
  under = "0"
) {
  paste0(
    formatC(contest, width = 7L, flag = "0"),
    formatC(voter, width = 9L, flag = "0"),
    formatC(serial, width = 7L, flag = "0"),
    formatC(tally, width = 3L, flag = "0"),
    formatC(precinct, width = 7L, flag = "0"),
    formatC(rank, width = 3L, flag = "0"),
    formatC(candidate, width = 7L, flag = "0"),
    over,
    under
  )
}

wineds_lookup_line <- function(type, id, description) {
  paste0(
    formatC(type, width = 10L, flag = "-"),
    formatC(id, width = 7L, flag = "0"),
    formatC(description, width = 50L, flag = "-"),
    "0000001",
    "0000100",
    "0",
    "0"
  )
}

wineds_fixture <- function() {
  lookup <- c(
    wineds_lookup_line("Contest", "0000100", "Mayor"),
    wineds_lookup_line("Candidate", "0000101", "ALICE"),
    wineds_lookup_line("Candidate", "0000102", "BOB"),
    wineds_lookup_line("Precinct", "0000001", "P1"),
    wineds_lookup_line("Tally Type", "0000001", "VBM")
  )
  ballot <- c(
    wineds_ballot_line("0000100", "000000001", "0000001", "001", "0000101"),
    wineds_ballot_line("0000100", "000000001", "0000001", "002", "0000102"),
    wineds_ballot_line("0000100", "000000002", "0000001", "001", "0000102"),
    wineds_ballot_line("0000100", "000000002", "0000001", "002", "0000101"),
    wineds_ballot_line("0000100", "000000003", "0000001", "001", "0000102")
  )
  list(ballot = ballot, lookup = lookup)
}

test_that("read_wineds() resolves labels into canonical marks", {
  fixture <- wineds_fixture()

  marks <- read_wineds(fixture$ballot, fixture$lookup)

  expect_s3_class(marks, "cvr_marks")
  expect_identical(marks$contest_id, rep("Mayor", 5L))
  expect_identical(
    marks$ballot_id,
    c("000000001", "000000001", "000000002", "000000002", "000000003")
  )
  expect_identical(marks$candidate_id, c("ALICE", "BOB", "BOB", "ALICE", "BOB"))
  expect_identical(marks$rank, c(1L, 2L, 1L, 2L, 1L))
  expect_identical(marks$precinct_id, rep("P1", 5L))
  expect_identical(marks$is_overvote, rep(FALSE, 5L))
  expect_identical(marks$is_skipped, rep(FALSE, 5L))
})

test_that("read_wineds() turns a flagged mark into a logical flag", {
  fixture <- wineds_fixture()
  fixture$ballot[1L] <- wineds_ballot_line(
    "0000100",
    "000000001",
    "0000001",
    "001",
    "0000101",
    over = "1"
  )

  marks <- read_wineds(fixture$ballot, fixture$lookup)

  expect_true(marks[ballot_id == "000000001" & rank == 1L, is_overvote])
})

test_that("read_wineds() tabulates the fixture end to end", {
  fixture <- wineds_fixture()

  marks <- read_wineds(fixture$ballot, fixture$lookup)
  result <- tabulate_irv(marks, sf_2018_rules())

  expect_identical(
    sort(unique(marks$ballot_id)),
    c(
      "000000001",
      "000000002",
      "000000003"
    )
  )
  expect_identical(result$winner, "BOB")
})

test_that("read_wineds() reads a header line when asked", {
  fixture <- wineds_fixture()
  ballot <- c("header", fixture$ballot)
  lookup <- c("header", fixture$lookup)

  marks <- read_wineds(ballot, lookup, b_header = TRUE, l_header = TRUE)

  expect_identical(nrow(marks), 5L)
})
