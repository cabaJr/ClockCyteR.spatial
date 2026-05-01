# collection of utils functions

# fix path
#' Convert backslash path separators to forward slashes
#'
#' @param x Character; file path string with backslash separators.
#'
#' @return Character; path with all backslashes replaced by forward slashes.
#' @keywords internal
back_to_forw <- function(x) {
  stringr::str_replace_all(x, "\\\\", "//")
}

#function to concatenate circular statistics results
concatCirc <- function(table, record, filename, pos) {
  # bind data
  table <- rbind.data.frame(table, record)
  # remove first empty row
  if (is.na(table[1, 1])) {
    table = table[-1, ]
  }
  # change rowname
  rownames(table)[dim(table)[1]] <- filename
  return(table)
}

#' Build the file path for a single plot output
#'
#' @param file_id Character; file identifier used as the base of the filename.
#' @param ch_id Character; channel identifier (e.g. \code{"ch1"}).
#' @param ch_id_folder Character; directory where the plot file will be saved.
#' @param plot_name Character; plot-type name appended to the filename.
#' @param save_format Character; file extension (e.g. \code{"svg"}).
#'
#' @return Character; full file path for the plot.
#' @export
build_plot_path <- function(file_id,
                            ch_id,
                            ch_id_folder,
                            plot_name,
                            save_format) {
  '
  params,
  file_id
  interval_name,
  ch_id,
  plot_type,
  ch_id_folder
  plot_obj$plot_name
  params$plotting$save_format
  '

  path <- file.path(ch_id_folder, paste0(file_id, "_",
                                         ch_id, "_",
                                         plot_name, ".",
                                         save_format
                                         ))
  return(path)
}

#' Path to the toy dataset folder
#'
#' @return the path to the toy dataset folder
#' @export
clockcyter_example_path <- function() {
  system.file("extdata", "Toy_dataset", package = "ClockCyteR.spatial")
}

#' Copy the toy dataset to a writable location
#'
#' @param dest Character; directory where the toy dataset will be copied.
#'   Defaults to a \code{ClockCyteR_example} subdirectory inside
#'   \code{tempdir()}.
#'
#' @return Character; path to the copied dataset folder (invisibly).
#' @export
setup_example <- function(dest = file.path(tempdir(), "ClockCyteR_example")) {
  src <- clockcyter_example_path()
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  file.copy(
    list.files(src, full.names = TRUE),
    dest,
    recursive = TRUE,
    overwrite = FALSE
  )
  message("Toy dataset copied to: ", dest)
  invisible(dest)
}

#' Create an empty list to accumulate FFT analysis results
#'
#' @return A list with empty vectors for each result field:
#'   \code{variance_vec}, \code{analysed_names_vec}, \code{reporter_vec},
#'   \code{traces_no_vec}, \code{tot_traces}, \code{vector_len},
#'   \code{period_var}, \code{period_mean}, \code{amplitude_mean},
#'   \code{error_mean}.
#' @keywords internal
createFFTtbl <- function() {
  #define df to store variance
  variance_vec = vector()
  analysed_names_vec = vector()
  reporter_vec = vector()
  traces_no_vec = vector()
  tot_traces = vector()
  vector_len = vector()
  period_var = vector()
  period_mean = vector()
  amplitude_mean = vector()
  error_mean = vector()
  return(list(
    variance_vec,
    analysed_names_vec,
    reporter_vec,
    traces_no_vec,
    tot_traces,
    vector_len,
    period_var,
    period_mean,
    amplitude_mean,
    error_mean
  ))
}

#' Create a subdirectory if it does not already exist
#'
#' @param base_dir Character; parent directory path.
#' @param folder_name Character; name of the subdirectory to create.
#'
#' @return Character; full path to the (now existing) subdirectory.
#' @keywords internal
create_folder <- function(base_dir, folder_name) {
  folder_path = file.path(base_dir, folder_name)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  return(folder_path)
}

#' Extract variables from analysis results across files and save as wide tables
#'
#' @param params List of analysis parameters produced by \code{make_params()};
#'   must contain \code{paths$base_dir}, \code{time$intervals}, and
#'   \code{channels}.
#' @param file_rows Data frame of file metadata; must contain columns
#'   \code{folder_path} and \code{file_id}.
#' @param vars Character vector of column names to extract from each result
#'   table.
#' @param network Logical; if \code{TRUE}, reads network result RDS files
#'   instead of period-table RDS files. Defaults to \code{FALSE}.
#' @param individual Logical; if \code{TRUE}, saves one row per cell instead
#'   of pivoting to a wide table. Defaults to \code{FALSE}.
#' @param ... Optional: \code{sub} (character) selects a sub-element from
#'   network results.
#'
#' @return Called for its side effect of writing CSV tables to a \code{tables}
#'   subdirectory; returns \code{NULL} invisibly.
#' @export
extract_vars <- function(params, file_rows, vars, network = FALSE, individual = FALSE, ...) {

  args <- list(...)
  sub <- args$sub

  tables_folder <- file.path(params$paths$base_dir, "tables")

  if (!dir.exists(tables_folder)) dir.create(tables_folder, recursive = TRUE)

  # process intervals
  purrr::walk(names(params$time$intervals), function(interval_name) {

    interval_range <- params$time$intervals[[interval_name]]

    # process channels
    purrr::walk(names(params$channels), function(ch_id) {

      if (!params$channels[[ch_id]]$enabled) return(NULL)

      # create file path and import rds file
      if(network){
        tbl_array <- file.path(file_rows$folder_path, "rds",
                               paste0(file_rows$file_id, "_",
                                      interval_name, "_",
                                      ch_id, "_",
                                      "network_results.rds"))
      } else {
        tbl_array <- file.path(file_rows$folder_path, "rds",
                                    paste0(file_rows$file_id, "_",
                                           interval_name, "_",
                                           ch_id, "_",
                                           "period_tbl.rds"))
      }

      data_combined <- lapply(tbl_array, function(tbl_path) {

        table <- readRDS(tbl_path)
        if(network){
          table <- table$network_vars[[{{sub}}]]
        }
        filename <- file_rows$file_id[
          vapply(file_rows$file_id,
                 function(id) grepl(paste0(id, "(?=[^0-9]|$)"), tbl_path, perl = TRUE),
                 logical(1))
        ]
        table$filename <- filename
        return(table)
      })
      if(individual) {} else {data_combined <- data_combined |> dplyr::bind_rows()}

      # For each variable, extract table
      if(individual) {
        table <- lapply(data_combined, function(tab){

          data.frame(filename = tab$filename,
                     variable = tab[[{{vars}}]])
        }) |> dplyr::bind_rows()

        colnames(table) <- c("filename", vars)

          # save table
          table_filepath <- file.path(tables_folder,
                                      paste0(interval_name, "_",
                                             ch_id, "_",
                                             vars, "_nested_table.csv"))
        write.csv(table, file = table_filepath)
      } else {
      purrr::walk(vars, function(var) {

        table_wide <- data_combined |>
          dplyr::select(filename, {{ var }}) |>
          mutate(row_id = ave(filename, filename, FUN = seq_along)) |>
          tidyr::pivot_wider(
            names_from = filename,
            values_from = {{ var }},
            values_fill = NA
          ) |>
          dplyr::select(-row_id)

        # save table
        table_filepath <- file.path(tables_folder,
                                    paste0(interval_name, "_",
                                           ch_id, "_",
                                           var, "_nested_table.csv"))
        write.csv(table_wide, file = table_filepath)
      })
    }
    })
  })
  message("Nested tables generated.")

}

check_pdf_deps <- function() {
  missing_pkgs <- character(0)
  if (!requireNamespace("magick",    quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "magick")
  if (!requireNamespace("tinytex",   quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "tinytex")
  if (!requireNamespace("gridExtra", quietly = TRUE)) missing_pkgs <- c(missing_pkgs, "gridExtra")

  if (length(missing_pkgs) > 0) {
    stop(
      "PDF report generation requires additional packages: ",
      paste(missing_pkgs, collapse = ", "), ".\n",
      "Install them with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  if (!tinytex::is_tinytex()) {
    stop(
      "TinyTeX (LaTeX) is not installed. PDF generation requires it.\n",
      "Install it with: tinytex::install_tinytex()\n",
      "Or use the default HTML output by leaving pdf = FALSE.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Render an Rmd report for a single file
#'
#' @param file_id Character; file identifier.
#' @param params List of analysis parameters.
#' @param base_dir Character; base directory of the project.
#' @param intervals Named list of analysis intervals.
#' @param channels Character vector of channel identifiers.
#' @param plot_types Character vector of plot-type keys to include in the
#'   report.
#' @param storage_fold Character; subdirectory containing the plots. Defaults
#'   to \code{"plots"}.
#' @param output_dir Character; directory where the rendered report is saved.
#'   Defaults to a \code{reports} subdirectory inside \code{base_dir}.
#' @param pdf Logical; if \code{TRUE} render a PDF instead of HTML. Requires
#'   \pkg{magick}, \pkg{tinytex}, and a TinyTeX installation. Defaults to
#'   \code{FALSE}.
#'
#' @return Called for its side effect of rendering a report; returns
#'   the output file path invisibly.
#' @keywords internal
generate_file_report <- function(
    file_id,
    params,
    base_dir,
    intervals,
    channels,
    plot_types,
    storage_fold = "plots",
    output_dir   = file.path(base_dir, "reports"),
    pdf          = FALSE
) {
  if (pdf) {
    check_pdf_deps()
    report_template <- "report_template_pdf.Rmd"
    output_ext      <- ".pdf"
  } else {
    report_template <- "report_template.Rmd"
    output_ext      <- ".html"
  }
  # Build period summary path
  period_summary_path <- file.path(base_dir, paste0(file_id, "_results"),
                                   "rds",
                                   paste0(file_id, "_period_table_summary.csv"))

  # Build plot paths: nested list [interval][channel][plot_type]
  plot_paths <- lapply(intervals, function(interval) {
    lapply(channels, function(channel) {
      if(!params$channels[[channel]]$enabled) return (NULL)
      lapply(plot_types, function(plot_type) {
        # Use your naming convention from pull_plots
        plot_name <- plot_type_to_name(plot_type)
        ch_id_folder <- file.path(base_dir,
                                  paste0(file_id, "_results"),
                                  "plots",
                                  channel)
        path <- build_plot_path(file_id = file_id,
                                ch_id = channel,
                                ch_id_folder,
                                plot_name = plot_name,
                                save_format = params$plotting$save_format)

      }) |> stats::setNames(plot_types)
    }) |> stats::setNames(channels)|> purrr::compact()
  }) |> stats::setNames(intervals)

  # Render the report
  rmarkdown::render(
    input = system.file("rmd", report_template, package = "ClockCyteR.spatial"),
    output_file = file.path(output_dir, paste0("report_", file_id, output_ext)),
    params = list(
      channel_params = params$channels,
      file_id = file_id,
      file_rows = file_rows,
      base_dir = params$paths$base_dir,
      period_summary_path = period_summary_path,
      plot_paths = plot_paths,
      intervals = intervals,
      channels = channels,
      plot_types = plot_types,
      save_format = params$plotting$save_format
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}

#' Import and scale grid centroid coordinates
#'
#' @param folder Character; path to the folder containing the CSV file.
#' @param filename Character; name of the CSV file with grid centroids.
#'   Defaults to \code{"grid_centroids.csv"}.
#' @param pixel_fct Numeric; pixel-to-micrometre conversion factor. Defaults
#'   to \code{2.82}.
#'
#' @return A data frame with columns \code{ID}, \code{x}, and \code{y}
#'   (coordinates in micrometres).
#'
#' @export
get_grid_coords <- function(
  folder,
  filename = "grid_centroids.csv",
  pixel_fct = 2.82
) {
  # import file with grid coordinates
  grid_coord_path = file.path(folder, filename)
  grid_coord = utils::read.csv(grid_coord_path)

  # Convert centroids to micrometers
  grid_coord$X <- grid_coord$X * pixel_fct
  grid_coord$Y <- grid_coord$Y * pixel_fct

  grid_coord <- grid_coord %>% `colnames<-`(c("ID", "x", "y"))

  return(grid_coord)
}

#' Bind a list of data frames into a single data frame
#'
#' @param ... Data frames to combine; all must have the same column names and
#'   number of columns.
#'
#' @return A single data frame with all rows from the input data frames.
#' @keywords internal
list2table <- function(...) {
  # get the list of dataframes
  df_list <- list(...)

  # check if the list is empty
  if (length(df_list) == 0) {
    stop("No dataframes provided in the list.")
  }

  # check if all dataframes hav the same number of columns
  num_cols <- sapply(df_list, ncol)
  if (length(unique(num_cols)) > 1) {
    stop("All dataframes must have the same number of columns.")
  }
  # check if all dataframes have the same column names
  col_names <- sapply(df_list, colnames)
  if (length(unique(col_names)) > 1) {
    stop("All dataframes must have the same column names.")
  }

  # combine all dataframes into one
  combined_df <- do.call(rbind, df_list)

  # return the combined dataframe
  return(combined_df)
}

#' Coerce circular phase columns to numeric in a summary table
#'
#' @param df Data frame with columns \code{phase_rad_mean} and
#'   \code{circ_phase_mean}.
#'
#' @return The input data frame with the two phase columns cast to
#'   \code{numeric}.
#' @keywords internal
normalize_summary <- function(df) {
  df |>
    dplyr::mutate(
      phase_rad_mean  = as.numeric(phase_rad_mean),
      circ_phase_mean = as.numeric(circ_phase_mean)
    )
}

#' Calculate plot height from type and width
#'
#' @param plot_type Character; plot-type key (see \code{plot_group()}).
#' @param plot_width Numeric; plot width in pixels.
#' @param ... Optional: \code{cell_coords} (data frame with column \code{Y})
#'   for \code{"spatial"} plots; \code{node_data} (data frame with column
#'   \code{Y}) for \code{"network_spatial"} plots.
#'
#' @return Numeric; recommended plot height in pixels.
#' @keywords internal
plot_height <- function(plot_type, plot_width, ...) {
  args <- list(...)
  cell_coords <- args$cell_coords
  node_data <- args$node_data

  group <- plot_group(plot_type)

  switch(group,
         "square" = {
           plot_height = plot_width
         },
         "tall" = {
           plot_height = 1.25*plot_width
         },
         "spatial" = {
           plot_height = round(length(unique(cell_coords$Y))*22.64)+190
         },
         "network_spatial" = {
           plot_height = round(length(unique(node_data$Y))*22.64)+210
         }
  )

  return(plot_height)
}

#' Map a plot-type key to its aspect-ratio group
#'
#' @param plot_type Character; plot-type key (e.g. \code{"period"},
#'   \code{"phase_distribution"}, \code{"spatial_clusters"}).
#'
#' @return Character; one of \code{"square"}, \code{"tall"},
#'   \code{"spatial"}, or \code{"network_spatial"}.
#' @keywords internal
plot_group <- function(plot_type){
  plot_groups <- list(
    square = c("period_hist", "degree_hist", "strength_hist",
               "degree_weight_hist", "cluster_hist", "overlap_hetmap",
               "corr_plot", "clusters_doughnut"),

    tall = c("phase_distribution", "two_phases_distribution", "clusters_phases",
             "clusters_period", "clusters_amplitude", "clusters_per_crn",
             "period_variance", "phase_variance"),

    spatial = c("period", "amplitude", "error", "RAE", "AUC",
             "phase", "phase_norm", "phase_circ",
             "period_crn", "amplitude_crn", "phase_crn", "spatial_node_weights",
             "spatial_node_strengths", "spatial_node_clust_coeff"),

    network_spatial = c("spatial_clusters")
  )

  plot_to_group <- stats::setNames(
    rep(names(plot_groups), lengths(plot_groups)),
    unlist(plot_groups)
  )

  group <- plot_to_group[[plot_type]]

  return(group)
}

#' Translate a plot-type key to its output filename stem
#'
#' @param plot_type Character; plot-type key.
#'
#' @return Character; the corresponding filename stem, or \code{NA} if
#'   \code{plot_type} is not in the dictionary.
#' @export
plot_type_to_name <- function(plot_type){

  dictionary <- c(
  "phase_distribution"   = "phase_distr",
  "period"              = "spatial_period",
  "amplitude"           = "spatial_amplitude",
  "error"               = "spatial_error",
  "RAE"                 = "spatial_RAE",
  "AUC"                 = "spatial_AUC",
  "phase"               = "spatial_phases",
  "phase_norm"          = "spatial_phases_norm",
  "phase_circ"          = "spatial_phases_circ",
  "period_crn"          = "spatial_period_crn",
  "amplitude_crn"       = "spatial_amplitude_crn",
  "phase_crn"           = "spatial_phase_crn",
  "3D"                  = "3D_period_phase",
  "phase_circular"      = "rayleigh",
  "two_phases_distribution" = "two_phases_distr",
  "period_hist"         = "period_histogram",
  "period_variance"     = "period_var",
  "phase_variance"      = "phase_var",
  "degree_hist"         = "degree_hist",
  "strength_hist"       = "strength_hist",
  "degree_weight_hist"  = "degree_weight_hist",
  "spatial_node_weights"   = "spatial_node_weights",
  "spatial_node_strengths" = "spatial_node_strengths",
  "spatial_node_clust_coeff" = "spatial_node_clust_coeff",
  "spatial_clusters"   = "spatial_clusters",
  "clusters_phases"    = "clusters_phases",
  "clusters_period"    = "clusters_period",
  "clusters_amplitude"  = "clusters_amplitude",
  "clusters_per_crn"   = "clusters_per_crn",
  "cluster_hist"       = "cluster_hist",
  "clusters_doughnut"  = "clusters_doughnut",
  "corr_plot"          = "corr_plot",
  "overlap_hetmap"     = "overlap_hetmap"
)
  plot_name <- dictionary[plot_type]
 return(plot_name)
}


# Function to print analysis settings
#' Print a summary of the current analysis settings to the console
#'
#' @return Called for its side effect of printing messages; returns
#'   \code{NULL} invisibly.
#' @keywords internal
print_analysis_settings <- function() {
  message("\n===== ANALYSIS SETTINGS =====\n")

  message(
    "\u27a4 Clean variables after period calculation: ",
    ifelse(clean_vars, "YES", "NO")
  )

  message(
    "\n\u27a4 Channel 1 Analysis: ",
    ifelse(Channel1_an, "ENABLED", "DISABLED")
  )
  if (Channel1_an) {
    message("    \u2022 Reporter: ", Ch1_rep)
    message("    \u2022 Invert signal: ", ifelse(invert_Ch1, "YES", "NO"))
  }

  message(
    "\n\u27a4 Channel 2 Analysis: ",
    ifelse(Channel2_an, "ENABLED", "DISABLED")
  )
  if (Channel2_an) {
    message("    \u2022 Reporter: ", Ch2_rep)
    message("    \u2022 Invert signal: ", ifelse(invert_Ch2, "YES", "NO"))
  }

  message(
    "\n\u27a4 Normalize Channel 2 phase using Channel 2: ",
    ifelse(normalize2, "YES", "NO")
  )

  message(
    "\n\u27a4 Generate comparison plots: ",
    ifelse(comparison_plots, "YES", "NO")
  )
  message("\u27a4 Generate phase plots: ", ifelse(phase_plots, "YES", "NO"))

  message(
    "\n\u27a4 Time Window Analysis: ",
    ifelse(time_wind, "ENABLED", "DISABLED")
  )
  if (time_wind && length(intervals) > 0) {
    message("    \u2022 Intervals to analyze:")
    for (int_name in names(intervals)) {
      message(
        "      - ",
        int_name,
        ": frames ",
        paste(intervals[[int_name]], collapse = " to ")
      )
    }
  }

  message("\n\u27a4 Time resolution: ", time_res, " hours per frame")
  message("\u27a4 Pixel-to-micrometer factor: ", pixel_fct)

  message(
    "\n\u27a4 Retrieve y-axis limits from previous experiment: ",
    ifelse(Y_limits_exp, "YES", "NO")
  )
  if (Y_limits_exp && !is.null(previous_exp)) {
    message("    \u2022 Previous experiment path: ", previous_exp)
  }

  message("\n\u27a4 Override ranges: ", ifelse(override_ranges, "YES", "NO"))
  if (exists("period_range_ch2")) {
    message(
      "    \u2022 Period range Channel 2: ",
      paste(period_range_ch2, collapse = " - ")
    )
  }

  message("\n\u27a4 Working directory: ", wd)
  message("\u27a4 Number of TIFF files found: ", length(files))
  message("\u27a4 Output folders will be created for each file")

  message("\n\u27a4 Save results: ", ifelse(saving, "YES", "NO"))
  message(
    "\u27a4 Recalculate existing period data: ",
    ifelse(RELOADPER, "YES", "NO")
  )

  message("\n=============================\n")
}

#' Copy RDS result files into a centralised output directory
#'
#' @param params List of analysis parameters produced by \code{make_params()};
#'   must contain \code{paths$base_dir}, \code{time$intervals}, and
#'   \code{channels}.
#' @param file_rows Data frame of file metadata; must contain columns
#'   \code{folder_path} and \code{file_id}.
#'
#' @return Called for its side effect of copying files; returns \code{NULL}
#'   invisibly.
#' @export
pull_rds <- function(params, file_rows) {

  # create folder "plots" inside the base directory
  base_dir <- params$paths$base_dir
  result_dir <- file.path(base_dir, "rds")
  if (!dir.exists(result_dir)) dir.create(result_dir)

  results_types <- c("period_tbl_clean", "auc_results")

  # for each plot activated in the params$plotting$plot_types
  purrr::walk(results_types, function(results_type){

    # make a new folder with the plot_type
    subresult_dir <- file.path(result_dir, results_types)
    if(!dir.exists(subresult_dir)) dir.create(subresult_dir)

    # Iterate through files, intervals, channels
    purrr::walk(1:nrow(file_rows), function(i) {
      file_row <- as.list(file_rows[i, ])
      intervals <- names(params$time$intervals)
      channels <- names(params$channels)


      purrr::walk(intervals, function(interval_name) {
        purrr::walk(channels, function(ch_id) {
          if (!params$channels[[ch_id]]$enabled) return(NULL)

          # Build original plot path
          save_format <- "svg"
          rds_folder <- file.path(file_row$folder_path, "rds")
          results_type <- results_type
          orig_result_path <- file.path(rds_folder,
                                        paste0(file_row$file_id, "_",
                                               interval_name, "_",
                                               ch_id, "_",
                                               results_type, ".rds"
                                                           ))

          # Destination path in new folder
          dest_result_path <- file.path(subresult_dir, basename(orig_result_path))

          # Copy plot file if it exists
          if (file.exists(orig_result_path)) {
            file.copy(orig_result_path, dest_result_path, overwrite = TRUE)
          } else {
            message(paste0("Could not copy \n", orig_result_path, "\nfile missing."))
          }
        })
      })
    })
  })
}

#' Copy plot files into a centralised output directory
#'
#' @param params List of analysis parameters produced by \code{make_params()};
#'   must contain \code{paths$base_dir}, \code{time$intervals},
#'   \code{channels}, and \code{plotting$plot_types}.
#' @param file_rows Data frame of file metadata; must contain columns
#'   \code{folder_path} and \code{file_id}.
#'
#' @return Called for its side effect of copying files; returns \code{NULL}
#'   invisibly.
#' @export
pull_plots <- function(params, file_rows){

  # create folder "plots" inside the base directory
  base_dir <- params$paths$base_dir
  plots_dir <- file.path(base_dir, "plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir)

  plot_types <- c(params$plotting$plot_types,
                  params$plotting$network_plots_types)
  save_format <- params$plotting$save_format

  # Check if multiple intervals
  multiple_intervals <- length(params$time$intervals) > 1

  # for each plot activated in the params$plotting$plot_types
  purrr::walk(plot_types, function(plot_type){

    plot_name <- plot_type_to_name(plot_type)
    if (is.na(plot_name)) {
      warning("No plot name mapping found for plot type: ", plot_type)
      return(NULL)
    }

    # make a new folder with the plot_type
    subplot_dir <- file.path(plots_dir, plot_type)
    if(!dir.exists(subplot_dir)) dir.create(subplot_dir)

    # Iterate through files, intervals, channels
    purrr::walk(1:nrow(file_rows), function(i) {
      file_row <- as.list(file_rows[i, ])
      intervals <- names(params$time$intervals)
      channels <- names(params$channels)

      purrr::walk(intervals, function(interval_name) {
        purrr::walk(channels, function(ch_id) {
          if (!params$channels[[ch_id]]$enabled) return(NULL)

          # Build original plot path with conditional interval layer
          if (multiple_intervals) {
            ch_id_folder <- file.path(file_row$folder_path, "plots", interval_name, ch_id)
          } else {
            ch_id_folder <- file.path(file_row$folder_path, "plots", ch_id)
          }

          orig_plot_path <- build_plot_path(
            file_row$file_id,
            ch_id,
            ch_id_folder,
            plot_name,
            save_format
          )

          # Destination path - include interval in filename if multiple intervals
          if (multiple_intervals) {
            dest_filename <- paste0(
              tools::file_path_sans_ext(basename(orig_plot_path)),
              "_", interval_name,
              ".", tools::file_ext(basename(orig_plot_path))
            )
          } else {
            dest_filename <- basename(orig_plot_path)
          }

          dest_plot_path <- file.path(subplot_dir, dest_filename)

          # Copy plot file if it exists
          if (file.exists(orig_plot_path)) {
            file.copy(orig_plot_path, dest_plot_path, overwrite = TRUE)
          } else {
            message(paste0("Could not copy \n", orig_plot_path, "\nfile missing."))
          }
        })
      })
    })
  })
}


#' Standard deviation that returns 0 for vectors with fewer than two non-NA values
#'
#' @param x Numeric vector.
#' @param na.rm Logical; whether to remove \code{NA} values before computing.
#'   Defaults to \code{TRUE}.
#'
#' @return Numeric; the standard deviation, or \code{0} if fewer than two
#'   non-\code{NA} values are present.
#' @keywords internal
safe_sd <- function(x, na.rm = TRUE) {
  if (sum(!is.na(x)) < 2) return(0)
  sd(x, na.rm = na.rm)
}

#' Save a named list of ggplot objects to files
#'
#' @param obj_to_save Named list of \code{ggplot} objects to save.
#' @param basepath Character; directory where plots will be saved.
#' @param filename Character; optional prefix for the output filenames.
#'   Defaults to \code{""}.
#' @param extension Character; file extension / device type (e.g.
#'   \code{"svg"}, \code{"png"}).
#' @param p.width Numeric; output width in pixels. Defaults to \code{560}.
#' @param p.height Numeric; output height in pixels. Defaults to \code{1100}.
#'
#' @return Called for its side effect of writing files; returns \code{NULL}
#'   invisibly.
#' @keywords internal
savePlots = function(
  obj_to_save,
  basepath,
  filename = "",
  extension,
  p.width = 560,
  p.height = 1100
) {
  if (filename != "") {
    filename = paste(filename, "_", sep = "")
  }
  for (i in seq_along(obj_to_save)) {
    # Check if the list has names; if not, use a default numeric index
    plot_name <- if (!is.null(names(obj_to_save))) {
      names(obj_to_save)[i]
    } else {
      paste0("plot_", i)
    }
    # Create the file path
    plot_path = file.path(basepath, paste0(filename, plot_name, ".", extension))

    # Save the plot
    ggplot2::ggsave(
      plot_path,
      plot = obj_to_save[[i]],
      width = p.width,
      height = p.height,
      units = "px"
    )
  }
}

#' Save a single ggplot object to file
#'
#' @param plot A \code{ggplot} object to save.
#' @param saving_path Character; full file path for the output.
#' @param format Character; file extension / device type (e.g. \code{"svg"}).
#' @param p.width Numeric; output width in pixels. Defaults to \code{560}.
#' @param p.height Numeric; output height in pixels. Defaults to \code{1100}.
#'
#' @return Called for its side effect of writing a file; returns \code{NULL}
#'   invisibly.
#' @keywords internal
save_plot <- function(plot,
                     saving_path,
                     format,
                     p.width = 560,
                     p.height = 1100
                     ){

  # Save the plot
  ggplot2::ggsave(
    filename = saving_path,
    plot = plot,
    width = p.width,
    height = p.height,
    units = "px"
  )

}

#' Append new FFT results to an accumulator list
#'
#' @param old_list List; existing accumulator as returned by
#'   \code{createFFTtbl()}.
#' @param new_list List; new results to append; must contain elements
#'   \code{trace_no} and \code{trace_tot}.
#' @param filename Character; file identifier added to
#'   \code{analysed_names_vec}.
#'
#' @return Updated accumulator list.
#' @keywords internal
updateFFTtbl <- function(old_list, new_list, filename) {
  # variance_vec <- c(old_list$variance_vec, new_list$variance)
  traces_no_vec <- c(old_list$traces_no_vec, new_list$trace_no)
  tot_traces <- c(old_list$tot_traces, new_list$trace_tot)
  analysed_names_vec <- c(old_list$analysed_names_vec, filename)
  period_var = c(old_list$period_var, new_list$period_var)
  period_mean = c(old_list$period_mean, new_list$period_mean)
  # vector_len = c(old_list$vector_len, new_list$vector_len)
  amplitude_mean = c(old_list$amplitude_mean, new_list$amplitude_mean)
  error_mean = c(old_list$error_mean, new_list$error_mean)
  reporter_vec <- c(old_list$reporter_vec, strsplit(filename, "_")[[1]][3])
  return(list(
    #variance_vec,
    analysed_names_vec,
    reporter_vec,
    traces_no_vec,
    tot_traces,
    # vector_len,
    period_var,
    period_mean,
    amplitude_mean,
    error_mean
  ))
}

