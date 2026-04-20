# Tests for make_params() and validate_params()
# -----------------------------------------------

test_that("make_params returns a list with expected top-level keys", {
  p <- make_minimal_params()
  expect_type(p, "list")
  expect_true(all(c("channels", "time", "paths", "plotting") %in% names(p)))
})

test_that("make_params stores channels correctly", {
  p <- make_minimal_params()
  expect_true("ch2" %in% names(p$channels))
  expect_true(p$channels$ch2$enabled)
})

test_that("make_params sets time_res", {
  p <- make_minimal_params()
  expect_equal(p$time$time_res, 0.5)
})

test_that("make_params with multiple channels preserves all entries", {
  # TODO: build a two-channel params and check both are present
  skip("fixture not yet defined")
})

# --- validate_params ----------------------------------------------------------

test_that("validate_params passes for a well-formed params list", {
  p <- make_minimal_params()
  expect_no_error(validate_params(p))
})

test_that("validate_params errors when a required field is missing", {
  # TODO: remove a required field and expect an error/warning
  skip("decide which fields are strictly required")
})

test_that("validate_params errors when channel has no reporter", {
  # TODO: set reporter = NULL or "" and check error
  skip("not yet implemented")
})
