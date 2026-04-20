# Integration tests for the high-level pipeline (modules_foo.R)
# ---------------------------------------------------------------
# These tests exercise analyze_channel → analyze_interval → analyze_file →
# analyze_project in increasing scope. They use a temporary directory so no
# files are left on disk after the test suite.

# Helper: build a minimal channel context
make_channel_ctx <- function(n_cells = 4, n_frames = 96) {
  traces <- make_trace_matrix(n_cells = n_cells, n_frames = n_frames)
  list(
    grid_vals   = traces,
    grid_coord  = make_grid_coord(n_cells),
    invert      = FALSE,
    reporter    = "GFP",
    enabled     = TRUE
  )
}

# Helper: build a minimal file context
make_file_ctx <- function(tmp_dir, n_cells = 4) {
  grid_coord    <- make_grid_coord(n_cells)
  grid_pts_sf   <- sf::st_as_sf(grid_coord, coords = c("X", "Y"))
  # A bounding polygon that contains all grid points
  bb  <- sf::st_bbox(grid_pts_sf)
  scn <- sf::st_sfc(sf::st_polygon(list(matrix(c(
    bb["xmin"] - 1, bb["ymin"] - 1,
    bb["xmax"] + 1, bb["ymin"] - 1,
    bb["xmax"] + 1, bb["ymax"] + 1,
    bb["xmin"] - 1, bb["ymax"] + 1,
    bb["xmin"] - 1, bb["ymin"] - 1
  ), ncol = 2, byrow = TRUE))))
  list(
    file_id        = "test_file",
    file_path      = file.path(tmp_dir, "test_file.csv"),
    folder_path    = tmp_dir,
    grid_coord     = grid_coord,
    grid_points_sf = grid_pts_sf,
    scn_roi        = scn,
    channels       = list(
      ch2 = make_channel_ctx(n_cells = n_cells)
    )
  )
}

# --- analyze_channel ----------------------------------------------------------

test_that("analyze_channel returns period_table and summaries", {
  tmp <- withr::local_tempdir()
  ch_ctx   <- make_channel_ctx()
  file_ctx <- make_file_ctx(tmp)
  params   <- make_minimal_params()

  result <- analyze_channel(
    channel_name = "ch2",
    channel_ctx  = ch_ctx,
    file_ctx     = file_ctx,
    params       = params
  )

  expect_type(result, "list")
  expect_true("period_table" %in% names(result))
})

test_that("analyze_channel period_table has one row per rhythmic cell", {
  # TODO: tighten this once filterRAE is wired through params
  skip("define expected row count for the synthetic trace")
})

# --- analyze_interval ---------------------------------------------------------

test_that("analyze_interval returns a list keyed by channel", {
  tmp      <- withr::local_tempdir()
  file_ctx <- make_file_ctx(tmp)
  params   <- make_minimal_params()

  result <- analyze_interval(
    file_ctx       = file_ctx,
    interval_name  = "Full",
    interval_range = c(0, 48),
    params         = params
  )

  expect_type(result, "list")
  # channels are nested under result$channels
  expect_true("ch2" %in% names(result$channels))
})

# --- analyze_file -------------------------------------------------------------

test_that("analyze_file returns summaries element", {
  tmp      <- withr::local_tempdir()
  file_ctx <- make_file_ctx(tmp)
  params   <- make_minimal_params()

  result <- analyze_file(file_ctx = file_ctx, params = params)

  expect_type(result, "list")
  expect_true("summaries" %in% names(result))
})

test_that("analyze_file saves period RDS to folder_path/rds/", {
  # TODO: check that the expected .rds file exists after analyze_file()
  skip("define expected filename pattern")
})

# --- analyze_project ----------------------------------------------------------

test_that("analyze_project returns a project_results list", {
  skip("requires a more complete file_rows fixture with real-looking paths")
})

test_that("analyze_project writes summary CSV to base_dir/rds/", {
  skip("requires writable base_dir and full file_rows fixture")
})

# --- generate_plots -----------------------------------------------------------

test_that("generate_plots writes at least one SVG per enabled plot type", {
  # TODO: run on a pre-analysed project fixture and check output files
  skip("requires a completed analysis fixture")
})

# --- ranges_calculation -------------------------------------------------------

test_that("ranges_calculation returns a list keyed by channel", {
  # TODO: build a file_rows with pre-saved RDS files and run
  skip("requires pre-saved period RDS fixture")
})
