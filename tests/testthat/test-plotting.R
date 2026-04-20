# Tests for fine-grained plotting functions (plotting_foo.R)
# ------------------------------------------------------------
# Most plotting tests follow the same pattern:
#   1. call the function with a minimal fixture
#   2. check the return value is a ggplot (or NULL/invisible for file-writing fns)
#   3. spot-check key aesthetic mappings / layer types

# --- hist_plot ----------------------------------------------------------------

test_that("hist_plot returns a ggplot in histogram mode", {
  result <- hist_plot(
    var       = rnorm(50),
    bin_color = "steelblue",
    x_label   = "Value"
  )
  expect_s3_class(result, "ggplot")
})

test_that("hist_plot returns a ggplot in bar mode", {
  result <- hist_plot(
    var       = sample(letters[1:4], 50, replace = TRUE),
    bin_color = "salmon",
    x_label   = "Category",
    mode      = "bar"
  )
  expect_s3_class(result, "ggplot")
})

test_that("hist_plot with xlims does not error", {
  result <- hist_plot(
    var       = rnorm(50),
    bin_color = "grey50",
    x_label   = "Value",
    xlims     = c(-3, 3, 1)
  )
  expect_s3_class(result, "ggplot")
})

# --- cluster_plot -------------------------------------------------------------

test_that("cluster_plot returns a ggplot", {
  df <- data.frame(
    cluster = factor(rep(c("1", "2"), each = 10)),
    period  = c(rnorm(10, 24), rnorm(10, 22)),
    color   = rep(c("blue", "red"), each = 10)
  )
  result <- cluster_plot(
    data       = df,
    x_var      = "cluster",
    y_var      = "period",
    color_var  = "cluster",
    colorscale = c("1" = "blue", "2" = "red")
  )
  expect_s3_class(result, "ggplot")
})

# --- doughnut_plot ------------------------------------------------------------

test_that("doughnut_plot returns a ggplot", {
  node_data <- data.frame(
    cluster = factor(c("1", "1", "2", "NULL"), levels = c("1", "2", "NULL"))
  )
  color_map <- assign_cluster_colors(node_data)
  result    <- doughnut_plot(node_data, color_map)
  expect_s3_class(result, "ggplot")
})

test_that("doughnut_plot handles a single-cluster input without error", {
  node_data <- data.frame(
    cluster = factor(rep("1", 5), levels = c("1", "NULL"))
  )
  color_map <- assign_cluster_colors(node_data)
  expect_no_error(doughnut_plot(node_data, color_map))
})

# --- spatial_clusters_plot ----------------------------------------------------

test_that("spatial_clusters_plot returns a ggplot", {
  node_data <- data.frame(
    X       = c(0, 50, 100),
    Y       = c(0, 0, 0),
    cluster = factor(c("1", "1", "2"), levels = c("1", "2"))
  )
  colors <- c("1" = "blue", "2" = "red")
  result <- spatial_clusters_plot(node_data, colors, filename = "test")
  expect_s3_class(result, "ggplot")
})

# --- highlight_cells_period ---------------------------------------------------

test_that("highlight_cells_period returns a ggplot", {
  all_cells  <- make_grid_coord(n_cells = 4)
  period_tbl <- make_period_table(n_cells = 4)
  result <- highlight_cells_period(
    all_cells    = all_cells,
    period_table = period_tbl,
    variable     = "period"
  )
  expect_s3_class(result, "ggplot")
})

test_that("highlight_cells_period_circ returns a ggplot", {
  all_cells  <- make_grid_coord(n_cells = 4)
  period_tbl <- make_period_table(n_cells = 4)
  result <- highlight_cells_period_circ(
    all_cells    = all_cells,
    period_table = period_tbl,
    variable     = "phase_rad"
  )
  expect_s3_class(result, "ggplot")
})

# --- phase_distribution -------------------------------------------------------

test_that("phase_distribution returns a ggplot for single channel", {
  phases_df <- data.frame(
    phase_norm = as.numeric(circular::circular(runif(30, 0, 2 * pi),
                                               units = "radians"))
  )
  result <- phase_distribution(data = phases_df)
  expect_s3_class(result, "ggplot")
})

test_that("phase_distribution returns a ggplot in multi-channel mode", {
  make_phase_df <- function(n = 30)
    data.frame(phase_norm = as.numeric(
      circular::circular(runif(n, 0, 2 * pi), units = "radians")
    ))
  result <- phase_distribution(data = make_phase_df(), multi = TRUE,
                               data2 = make_phase_df())
  expect_s3_class(result, "ggplot")
})

# --- plot_group_traces --------------------------------------------------------

test_that("plot_group_traces returns a ggplot", {
  summary_data <- data.frame(
    t     = rep(seq(0, 46, by = 2), 2),
    mean  = rnorm(48),
    se    = runif(48, 0, 0.1),
    group = rep(c("A", "B"), each = 24)
  )
  result <- plot_group_traces(summary_data)
  expect_s3_class(result, "ggplot")
})

# --- plot_overlap_heatmap -----------------------------------------------------

test_that("plot_overlap_heatmap returns a ggplot", {
  mat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2,
                dimnames = list(c("1", "2"), c("1", "2")))
  result <- plot_overlap_heatmap(mat)
  expect_s3_class(result, "ggplot")
})

# --- plot_cell ----------------------------------------------------------------

test_that("plot_cell returns a ggplot in wide mode", {
  traces <- make_trace_matrix(n_cells = 3, n_frames = 48)
  result <- plot_cell(cellID = "1", data = traces, tbl.type = "wide")
  expect_s3_class(result, "ggplot")
})

test_that("plot_cell returns a ggplot in long mode", {
  # TODO: build a long-format data frame with ID / t / value columns
  skip("define long-format trace fixture")
})

# --- assign_cluster_colors ----------------------------------------------------

test_that("assign_cluster_colors returns a data frame with cluster and color", {
  node_data <- data.frame(
    cluster = factor(c("1", "2", "NULL"), levels = c("1", "2", "NULL"))
  )
  result <- assign_cluster_colors(node_data)
  expect_true(all(c("cluster", "color") %in% names(result)))
  expect_equal(nrow(result), 3)
})

test_that("assign_cluster_colors assigns grey80 to NULL cluster", {
  node_data <- data.frame(
    cluster = factor(c("1", "NULL"), levels = c("1", "NULL"))
  )
  result <- assign_cluster_colors(node_data)
  expect_equal(result$color[result$cluster == "NULL"], "grey80")
})

test_that("assign_cluster_colors handles more clusters than palette size", {
  # TODO: create 31+ cluster levels and verify recycling without error
  skip("define large-cluster fixture")
})

# --- circular_plot (file-writing) ---------------------------------------------
# circular_plot uses base graphics and calls dev.off() internally.
# When saving = TRUE it opens its own SVG device; that is safe.
# When saving = FALSE it still calls dev.off(), so we pre-open a temporary
# PDF device to give it something to close — otherwise it would attempt to
# close the null device (device 1), which causes a hard C++ crash.

test_that("circular_plot writes an SVG file when saving = TRUE", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "test_circular.svg")
  phases <- circular::circular(runif(20, 0, 2 * pi), units = "radians")
  expect_no_error(
    circular_plot(data = phases, path = path, saving = TRUE)
  )
  expect_true(file.exists(path))
})

test_that("circular_plot does not write a file when saving = FALSE", {
  tmp    <- withr::local_tempdir()
  path   <- file.path(tmp, "should_not_exist.svg")
  phases <- circular::circular(runif(20, 0, 2 * pi), units = "radians")
  # Open a temporary device so circular_plot's internal dev.off() has a
  # non-null device to close.
  grDevices::pdf(file.path(tmp, "tmp_device.pdf"))
  circular_plot(data = phases, path = path, saving = FALSE)
  expect_false(file.exists(path))
})
