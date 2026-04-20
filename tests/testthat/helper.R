# Shared fixtures for all test files
# ------------------------------------
# Loaded automatically by testthat before any test file runs.

# Minimal synthetic trace: 2 cells × 96 frames (48 h at 30 min resolution)
make_trace_matrix <- function(n_cells = 2, n_frames = 96, period_h = 24,
                              time_res = 0.5, seed = 42) {
  set.seed(seed)
  t <- seq(0, by = time_res, length.out = n_frames)
  mat <- t(sapply(seq_len(n_cells), function(i) {
    phase_offset <- runif(1, 0, 2 * pi)
    sin(2 * pi * t / period_h + phase_offset) + rnorm(n_frames, sd = 0.05)
  }))
  rownames(mat) <- as.character(seq_len(n_cells))
  mat
}

# Minimal grid coordinates matching the trace matrix
make_grid_coord <- function(n_cells = 2) {
  data.frame(
    ID = seq_len(n_cells),
    X  = seq(0, by = 50, length.out = n_cells),
    Y  = rep(0, n_cells)
  )
}

# Minimal period table (as returned by computePeriod)
make_period_table <- function(n_cells = 2) {
  data.frame(
    ID          = seq_len(n_cells),
    period      = rep(24, n_cells),
    amplitude   = rep(1,  n_cells),
    phase_h     = rep(6,  n_cells),
    phase_rad   = rep(pi / 2, n_cells),
    RAE         = rep(0.1, n_cells),
    keep        = rep(TRUE, n_cells)
  )
}

# Minimal params list (subset sufficient for unit-level tests)
make_minimal_params <- function() {
  make_params(
    channels = list(
      ch2 = list(enabled = TRUE, reporter = "GFP", invert = FALSE,
                 label = "ch2", grid_file = "")
    ),
    time_res   = 0.5,
    pixel_fct  = 2.82,
    network    = FALSE,
    coherence  = FALSE,
    saving     = FALSE,
    plotting   = list(
      y_limits_from_previous = FALSE,
      plot_types   = c("period", "phase"),
      save_format  = "svg",
      ranges       = list(),
      colors       = list(ch2 = "darkgreen")
    )
  )
}
