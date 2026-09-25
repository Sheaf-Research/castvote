#' @noRd
null_default <- function(x, y) {
  if (is.null(x)) y else x
}

#' @noRd
dominion_int <- function(x) {
  if (is.null(x) || is.character(x)) {
    return(-1L)
  }
  as.integer(x)
}

#' @noRd
dominion_redacted <- function(marks) {
  if (length(marks) != 1L) {
    return(FALSE)
  }
  first <- marks[[1L]]
  is.character(first) && identical(first, "*** REDACTED ***")
}

#' @noRd
dominion_row <- function(
  session,
  block,
  card,
  contest,
  mark,
  original_modified,
  session_index,
  zero
) {
  list(
    originalModified = original_modified,
    sessionType = as.character(null_default(
      session$SessionType,
      NA_character_
    )),
    precinctPortionId = as.integer(null_default(
      block$PrecinctPortionId,
      NA_integer_
    )),
    ballotTypeId = as.integer(null_default(block$BallotTypeId, NA_integer_)),
    tabulatorId = as.integer(null_default(session$TabulatorId, NA_integer_)),
    batchId = as.integer(null_default(session$BatchId, NA_integer_)),
    recordId = as.character(null_default(session$RecordId, NA_character_)),
    countingGroupId = as.integer(null_default(
      session$CountingGroupId,
      NA_integer_
    )),
    sessionIndex = as.integer(session_index),
    isCurrent = as.logical(null_default(block$IsCurrent, NA)),
    cardId = as.integer(null_default(card$Id, NA_integer_)),
    paperIndex = as.integer(null_default(card$PaperIndex, NA_integer_)),
    contestId = as.integer(null_default(contest$Id, NA_integer_)),
    overvotes = dominion_int(contest$Overvotes),
    undervotes = dominion_int(contest$Undervotes),
    candidateId = if (zero) {
      -1L
    } else {
      as.integer(null_default(
        mark$CandidateId,
        -1L
      ))
    },
    partyId = if (zero || is.null(mark$PartyId)) {
      -1L
    } else {
      as.integer(
        mark$PartyId
      )
    },
    rank = if (zero) -1L else as.integer(null_default(mark$Rank, -1L)),
    isAmbiguous_mdens = if (zero) {
      -1L
    } else {
      as.integer(isTRUE(mark$IsAmbiguous)) *
        1000L +
        as.integer(null_default(mark$MarkDensity, 0L))
    },
    isVote = if (zero) FALSE else isTRUE(mark$IsVote)
  )
}

#' @noRd
dominion_session_rows <- function(session, session_index) {
  rows <- list()
  for (original_modified in c("Original", "Modified")) {
    block <- session[[original_modified]]
    if (is.null(block)) {
      next
    }
    cards <- block$Cards
    if (is.null(cards)) {
      if (is.null(block$Contests)) {
        next
      }
      cards <- list(list(
        Id = NA_integer_,
        PaperIndex = NA_integer_,
        Contests = block$Contests
      ))
    }
    for (card in cards) {
      contests <- card$Contests
      if (is.null(contests)) {
        next
      }
      for (contest in contests) {
        marks <- contest$Marks
        if (is.null(marks)) {
          marks <- list()
        }
        if (length(marks) == 0L || dominion_redacted(marks)) {
          rows[[length(rows) + 1L]] <- dominion_row(
            session,
            block,
            card,
            contest,
            NULL,
            original_modified,
            session_index,
            zero = TRUE
          )
        } else {
          for (mark in marks) {
            rows[[length(rows) + 1L]] <- dominion_row(
              session,
              block,
              card,
              contest,
              mark,
              original_modified,
              session_index,
              zero = FALSE
            )
          }
        }
      }
    }
  }
  rows
}

#' Extract Dominion marks from parsed CVR sessions
#'
#' Flattens the nested session, card, contest and mark structure into one row
#' per mark, mirroring the reference Dominion extraction.
#'
#' @param sessions The `Sessions` element of a parsed Dominion CVR file.
#' @return A data table of marks.
#' @noRd
dominion_extract_marks <- function(sessions) {
  if (is.null(sessions) || !length(sessions)) {
    return(data.table::data.table())
  }
  rows <- lapply(seq_along(sessions), function(i) {
    dominion_session_rows(sessions[[i]], i)
  })
  data.table::rbindlist(
    unlist(rows, recursive = FALSE),
    use.names = TRUE,
    fill = TRUE
  )
}

#' @noRd
dominion_manifest <- function(zip_path, name) {
  raw <- readr::read_file_raw(unz(zip_path, name))
  data.table::as.data.table(RcppSimdJson::fparse(raw)$List)
}

#' @noRd
dominion_lapply <- function(items, worker, cores = 1L) {
  if (!length(items)) {
    return(list())
  }
  if (cores > 1L && .Platform$OS.type == "unix") {
    parallel::mclapply(items, worker, mc.cores = min(cores, length(items)))
  } else {
    lapply(items, worker)
  }
}

#' @noRd
dominion_marks_from_entry <- function(zip_path, entry) {
  json <- tryCatch(
    RcppSimdJson::fparse(
      readr::read_file_raw(unz(zip_path, entry)),
      max_simplify_lvl = "list"
    ),
    error = function(err) {
      cv_log_warn("Skipping %s: %s", entry, conditionMessage(err))
      NULL
    }
  )
  if (is.null(json)) {
    return(NULL)
  }
  marks <- dominion_extract_marks(json$Sessions)
  if (nrow(marks)) {
    marks[, cvr_file := entry]
  }
  marks
}

#' Read a Dominion CVR export
#'
#' A Dominion export is a zip of `CvrExport_*.json` files plus manifest files
#' that name every internal id. Each session and card is one ballot sheet. Marks
#' are flattened and joined to the manifests, then reduced to the
#' [castvote_schema()] long format for recorded votes. Contest-level overvote
#' and undervote counts are carried through as extra columns; mapping them to
#' per-rank flags is not yet defined.
#'
#' @param zip_path Path to the CVR zip, read in place.
#' @param contests Contest ids to keep. Defaults to the ranked contests
#'   (`NumOfRanks > 0` or `VoteFor > 1`).
#' @param files Optional subset of zip entries, for testing.
#' @param cores Files read in parallel on unix.
#' @return Canonical marks as a data table.
#' @export
read_dominion <- function(zip_path, contests = NULL, files = NULL, cores = 1L) {
  checkmate::assert_file_exists(zip_path, access = "r")

  contest_man <- dominion_manifest(zip_path, "ContestManifest.json")
  candidate_man <- dominion_manifest(zip_path, "CandidateManifest.json")
  portion_man <- dominion_manifest(zip_path, "PrecinctPortionManifest.json")
  counting_man <- dominion_manifest(zip_path, "CountingGroupManifest.json")

  if (is.null(contests)) {
    contests <- contest_man[NumOfRanks > 0 | VoteFor > 1, Id]
  }
  if (is.null(files)) {
    entries <- utils::unzip(zip_path, list = TRUE)$Name
    files <- grep("^CvrExport.*\\.json$", entries, value = TRUE)
  }

  cv_log_info("Reading %s Dominion CVR file(s).", length(files))

  marks <- data.table::rbindlist(
    dominion_lapply(
      files,
      function(entry) dominion_marks_from_entry(zip_path, entry),
      cores = cores
    ),
    fill = TRUE
  )
  marks <- marks[contestId %in% contests]
  marks[, is_ambiguous := isAmbiguous_mdens > 100L]
  marks[,
    mark_density := isAmbiguous_mdens - 1000L * (isAmbiguous_mdens > 100L)
  ]
  marks[, isAmbiguous_mdens := NULL]
  marks[, ballot_id := paste(cvr_file, sessionIndex, cardId, sep = ".")]

  marks <- merge(
    marks,
    contest_man[, list(contestId = Id, contest = Description)],
    by = "contestId",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    candidate_man[, list(candidateId = Id, candidate = Description)],
    by = "candidateId",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    portion_man[, list(precinctPortionId = Id, precinct = Description)],
    by = "precinctPortionId",
    all.x = TRUE,
    sort = FALSE
  )
  marks <- merge(
    marks,
    counting_man[, list(countingGroupId = Id, counting_group = Description)],
    by = "countingGroupId",
    all.x = TRUE,
    sort = FALSE
  )

  marks <- marks[rank >= 1L & candidateId > 0L & isVote %in% TRUE]
  marks[, `:=`(is_overvote = FALSE, is_skipped = FALSE)]

  out <- marks[, list(
    ballot_id,
    contest_id = contest,
    precinct_id = precinct,
    rank = as.integer(rank),
    candidate_id = candidate,
    is_overvote,
    is_skipped,
    overvotes,
    undervotes,
    is_ambiguous,
    mark_density,
    cvr_file
  )]

  cv_log_info(
    "Read %s recorded-vote marks across %s contests from Dominion.",
    nrow(out),
    data.table::uniqueN(out$contest_id)
  )

  canonicalize_ballots(out)
}
