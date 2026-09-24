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
