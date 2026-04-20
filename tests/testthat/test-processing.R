# Tests for fine-grained processing functions
# ---------------------------------------------

# --- preprocess_data ----------------------------------------------------------

test_that("preprocess_data returns matrix with same dimensions as input", {
  mat <- make_trace_matrix(n_cells = 3, n_frames = 96)
  # preprocess_data expects time as first column
  input <- rbind(seq(0, by = 0.5, length.out = 96), mat)
  result <- preprocess_data(input, grade = 2)
  expect_equal(dim(result), dim(input))
})

test_that("preprocess_data does not introduce all-NA columns", {
  # TODO: run on a matrix with mild outliers and check no full-NA columns
  skip("define outlier fixture")
})

# --- prep_table ---------------------------------------------------------------

test_that("prep_table returns a data.table with at least columns t, ID, value", {
  mat <- make_trace_matrix(n_cells = 2, n_frames = 48)
  result <- prep_table(mat, time_res = 0.5, preprocess = FALSE, transp = TRUE,
                       add_t = TRUE)
  expect_true(all(c("t", "ID", "value") %in% names(result)))
})

test_that("prep_table has one row per cell per frame", {
  n_cells  <- 2
  n_frames <- 48
  mat    <- make_trace_matrix(n_cells = n_cells, n_frames = n_frames)
  result <- prep_table(mat, time_res = 0.5, preprocess = FALSE,
                       transp = TRUE, add_t = TRUE)
  expect_equal(nrow(result), n_cells * n_frames)
})

test_that("prep_table respects rm.start", {
  # TODO: check that the first rm.start frames are absent from the output
  skip("define expected row count after trimming")
})

# --- computePeriod ------------------------------------------------------------

test_that("computePeriod returns a list with period_table element", {
  mat    <- make_trace_matrix(n_cells = 2, n_frames = 96, period_h = 24)
  result <- computePeriod(mat, top = 30, bottom = 18, time_res = 0.5,
                          preprocess = FALSE)
  expect_type(result, "list")
  expect_true("period_table" %in% names(result))
})

test_that("computePeriod period_table has one row per cell", {
  n_cells <- 3
  mat    <- make_trace_matrix(n_cells = n_cells, n_frames = 96)
  result <- computePeriod(mat, top = 30, bottom = 18, time_res = 0.5,
                          preprocess = FALSE)
  expect_equal(nrow(result$period_table), n_cells)
})

test_that("computePeriod detects a 24 h period within tolerance", {
  # TODO: use a clean sinusoid and check estimated period ~24 h (± 1 h)
  skip("requires clean sinusoid fixture and tolerance check")
})

test_that("computePeriod filterRAE removes high-RAE cells", {
  # TODO: inject a flat (non-rhythmic) trace and verify it is filtered out
  skip("define flat-trace fixture")
})

# --- FFT_NLLS_analyse ---------------------------------------------------------

test_that("FFT_NLLS_analyse returns period, phase, amplitude, RAE", {
  # TODO: call with a single-cell long-format data frame and check list keys
  skip("define single-cell data frame fixture")
})

# --- circular_stats -----------------------------------------------------------

test_that("circular_stats returns expected keys", {
  pt <- make_period_table(n_cells = 5)
  result <- circular_stats(pt)
  expect_true(all(c("mean_phase", "vectorLength", "circStats") %in% names(result)))
})

test_that("circular_stats returns NA circStats for a single-cell table", {
  # TODO: check behaviour with n_cells = 1 (Rayleigh test degenerate case)
  skip("check edge case behaviour")
})

# --- normalize_phase ----------------------------------------------------------

test_that("normalize_phase returns values in [-12, 12]", {
  phases <- seq(-pi, pi, length.out = 20)
  result <- normalize_phase(phases)
  expect_true(all(result >= -12 & result <= 12, na.rm = TRUE))
})

test_that("normalize_phase returns empty vector for empty input", {
  expect_equal(length(normalize_phase(numeric(0))), 0)
})

# --- calculate_auc ------------------------------------------------------------

test_that("calculate_auc returns a single numeric", {
  y      <- sin(seq(0, 4 * pi, length.out = 100))
  result <- calculate_auc(y)
  expect_length(result, 1)
  expect_true(is.numeric(result))
})

test_that("calculate_auc normalised result is smaller than raw", {
  y   <- sin(seq(0, 4 * pi, length.out = 100)) + 2
  raw <- calculate_auc(y, normalize = FALSE)
  nrm <- calculate_auc(y, normalize = TRUE)
  expect_gt(raw, nrm)
})

# --- network_analysis ---------------------------------------------------------

test_that("network_analysis returns igraph_obj, node_data, network_vars", {
  # TODO: run with a small trace matrix and period_table; requires a tmp dir
  skip("needs temp directory fixture for network_dir")
})

test_that("network_analysis node_data has one row per cell", {
  skip("needs temp directory fixture for network_dir")
})
