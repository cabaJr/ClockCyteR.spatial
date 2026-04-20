#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## ggplot2 is imported wholesale because 30+ functions are used without ::
#' @import ggplot2
## circular -- functions used without :: prefix
#' @importFrom circular circular mean.circular angular.variance rayleigh.test
#'   as.circular conversion.circular minusPiPlusPi arrows.circular
#'   points.circular axis.circular
## dplyr -- pipe and tidy-eval helpers used without ::
#' @importFrom dplyr %>% group_by summarise summarize mutate filter select
#'   bind_rows across all_of left_join arrange case_when rename right_join n
## tidyr
#' @importFrom tidyr pivot_longer pivot_wider
## tidyselect -- where() used in across()
#' @importFrom tidyselect where
## pracma
#' @importFrom pracma trapz findpeaks
## scales -- used without :: in some plotting helpers
#' @importFrom scales rescale alpha squish
## rlang -- tidy-eval
#' @importFrom rlang sym !! enquo as_label
## igraph -- graph functions used without :: in network_foo.R / processing_foo.R
#' @importFrom igraph degree strength E V transitivity cluster_leiden
#'   membership modularity graph_from_data_frame delete.edges
#'   induced_subgraph ends subgraph_from_edges distances similarity contract
#'   simplify "V<-"
## tidygraph
#' @importFrom tidygraph as_tbl_graph
## sf -- spatial functions used without :: in processing_foo.R / modules_foo.R
#' @importFrom sf st_as_sf st_within st_combine st_convex_hull st_intersection
#'   st_geometry st_area st_union st_polygon st_sfc st_bbox
## cli -- progress bars
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
## plotly -- 3D and interactive plots
#' @importFrom plotly plot_ly add_trace layout
## scico -- colour palettes
#' @importFrom scico scico
## Base R packages -- functions called without :: prefix
#' @importFrom grDevices adjustcolor dev.off png rgb svg
#' @importFrom graphics legend lines plot.new points text title
#' @importFrom stats approx as.formula ave complete.cases cor density dist
#'   fft formula heatmap lm loess mad median predict quantile resid residuals
#'   runif sd setNames var weighted.mean
#' @importFrom utils data install.packages read.csv write.csv
## usethis namespace: end

## Suppress R CMD check NOTEs for column names used in dplyr/ggplot2 NSE
## and for bare variable names in legacy code that uses global scope.
utils::globalVariables(c(
  # dplyr / ggplot2 column references
  ".", "ID", "X", "Y", "alphaval", "amplitude", "channel",
  "circ_phase_mean", "clust_coef", "cluster", "file_id", "geometry",
  "group", "mean_phase", "n", "period", "period_crn", "percentage",
  "phase_circ", "phase_crn", "phase_norm", "phase_rad_mean", "row_id",
  "se", "value", "Cluster1", "Cluster2", "Segregation",
  # legacy / global-scope variables in old-style helper functions
  "Ch1_rep", "Ch2_rep", "Channel1_an", "Channel2_an", "RELOADPER",
  "Y_limits_exp", "clean_vars", "comparison_plots", "del", "eye",
  "file_rows", "filename", "filenames", "files", "grid_coord",
  "int_list", "intervals", "invert_Ch1", "invert_Ch2", "normalize2",
  "override_ranges", "params", "period_range_ch2", "period_tbl",
  "phase_plots", "pixel_fct", "previous_exp", "saving",
  "time_res", "time_wind", "wd"
))
NULL
