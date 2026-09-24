dominion_sessions <- function(json) {
  RcppSimdJson::fparse(json, max_simplify_lvl = "list")$Sessions
}

dominion_session_json <- '{
  "Sessions": [
    {
      "TabulatorId": 1,
      "BatchId": 2,
      "RecordId": 3,
      "CountingGroupId": 4,
      "SessionType": "Orig",
      "Original": {
        "PrecinctPortionId": 900,
        "BallotTypeId": 7,
        "IsCurrent": true,
        "Cards": [
          {
            "Id": 11,
            "PaperIndex": 0,
            "Contests": [
              {
                "Id": 100,
                "Overvotes": 0,
                "Undervotes": 0,
                "Marks": [
                  {"CandidateId": 101, "Rank": 1, "IsVote": true, "MarkDensity": 200, "IsAmbiguous": false},
                  {"CandidateId": 102, "Rank": 2, "IsVote": true, "MarkDensity": 150, "IsAmbiguous": false}
                ]
              }
            ]
          }
        ]
      }
    }
  ]
}'

test_that("dominion_extract_marks() flattens a session into one row per mark", {
  marks <- dominion_extract_marks(dominion_sessions(dominion_session_json))

  expect_identical(nrow(marks), 2L)
  expect_identical(marks$originalModified, c("Original", "Original"))
  expect_identical(marks$sessionType, c("Orig", "Orig"))
  expect_identical(marks$precinctPortionId, c(900L, 900L))
  expect_identical(marks$ballotTypeId, c(7L, 7L))
  expect_identical(marks$tabulatorId, c(1L, 1L))
  expect_identical(marks$cardId, c(11L, 11L))
  expect_identical(marks$contestId, c(100L, 100L))
  expect_identical(marks$candidateId, c(101L, 102L))
  expect_identical(marks$rank, c(1L, 2L))
  expect_true(all(marks$isVote))
})

test_that("dominion_extract_marks() records a mark density", {
  marks <- dominion_extract_marks(dominion_sessions(dominion_session_json))

  expect_identical(marks$isAmbiguous_mdens, c(200L, 150L))
})

test_that("dominion_extract_marks() emits one placeholder row for a redacted contest", {
  json <- '{
    "Sessions": [
      {
        "TabulatorId": 1,
        "BatchId": 2,
        "CountingGroupId": 4,
        "SessionType": "Orig",
        "Original": {
          "PrecinctPortionId": 900,
          "BallotTypeId": 7,
          "IsCurrent": true,
          "Cards": [
            {"Id": 11, "PaperIndex": 0, "Contests": [
              {"Id": 100, "Overvotes": 0, "Undervotes": 0, "Marks": ["*** REDACTED ***"]}
            ]}
          ]
        }
      }
    ]
  }'

  marks <- dominion_extract_marks(dominion_sessions(json))

  expect_identical(nrow(marks), 1L)
  expect_identical(marks$candidateId, -1L)
  expect_identical(marks$rank, -1L)
  expect_false(marks$isVote)
})
