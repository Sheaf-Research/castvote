test_that("ballot resolution helpers are exported", {
  expect_true(is.function(resolve_ballots))
  expect_true(is.function(validate_candidate_ids))
  expect_true(is.function(canonicalize_ballots))
})
