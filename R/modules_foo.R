# collection of modules functions

# Project layer ####

#' analyze_project
#'
#' Runs the full analysis pipeline across all files in a project, aggregating
#' results and saving summary outputs to disk.
#'
#' @param file_rows A tibble of file metadata with columns \code{file_id},
#'   \code{file_path}, and \code{folder_path}, as produced by \code{index_files()}.
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#'
#' @return A named list with elements \code{params} (the input parameter list),
#'   \code{file_results} (a list of per-file analysis results), and
#'   \code{file_contexts} (a list of per-file data contexts).
#' @export
#'
#' @examples
#' \dontrun{
#' results <- analyze_project(file_rows, params)
#' }
analyze_project <- function(file_rows, params) {

  # TODO consider adding index files inside the analyze project function

  # Log start of project analysis
  message(paste0("Starting project analysis"))
  message(paste0("Number of files: ", nrow(file_rows)))

  # Prepare file contexts (load all data for each file)

  file_contexts <- purrr::pmap(file_rows,
                              function(file_id, file_path, folder_path, ...) {

                                prepare_file(
                                  list(
                                    file_id = file_id,
                                    file_path = file_path,
                                    folder_path = folder_path
                                  ),
                                  params = params
                                  )
                                }
                              )

  # Analyze each file (includes file-level I/O and interval/channel analysis)
  project_results <- purrr::map(file_contexts, analyze_file, params = params)

  project_results$summary <- project_results |>
    purrr::map("summaries") |>
    purrr::map(normalize_summary) |>
    purrr::list_rbind()

  # save outputs
  rds_main <- file.path(params$paths$base_dir, "rds")
  if (!dir.exists(rds_main)) dir.create(rds_main, recursive = TRUE)

  write.csv(project_results$summary,
            file.path(rds_main,
                      paste0("analysis_summary_",
                             Sys.Date(),
                             ".csv")
            )
  )

  saveRDS(project_results$summary,
          file.path(rds_main,
                    paste0("analysis_summary_",
                           Sys.Date(),
                           ".rds")
          )
  )

  saveRDS(project_results,
          file.path(rds_main,
                    paste0("analysis_results_",
                           Sys.Date(),
                           ".rds")
          )
  )
  # Log finish of analysis
  message(paste0("Project analysis finished"))
  # Return all results and (optionally) summary paths
  list(
    params          = params,
    file_results    = project_results,
    file_contexts   = file_contexts
    # all_summaries = all_summaries
  )
}

# File layer ####

#' analyze_file
#'
#' Runs interval and channel analysis for a single file, saving per-channel
#' RDS outputs and a summary table to the file's results folder.
#'
#' @param file_ctx A named list of file context data as produced by
#'   \code{prepare_file()}, containing elements including \code{file_id},
#'   \code{folder_path}, \code{channels}, \code{grid_points_sf},
#'   \code{scn_roi}, and \code{grid_coord}.
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#'
#' @return A named list with elements \code{file_id} (character),
#'   \code{filename} (character), \code{intervals} (a named list of
#'   interval-level results), and \code{summaries} (a combined data frame
#'   of per-channel period summary statistics across all intervals).
#' @export
#'
#' @examples
#' \dontrun{
#' file_result <- analyze_file(file_ctx, params)
#' }
analyze_file <- function(file_ctx, params) {

  # Create results folder for this file
  rds_folder <- file.path(file_ctx$folder_path, "rds")
  if (!dir.exists(rds_folder)) dir.create(rds_folder, recursive = TRUE)

  # Log file that is being processed
  message(paste0(" |--- Processing file: ", file_ctx$file_id ))

  # Extract intervals from params
  intervals <- params$time$intervals
  if(is.null(intervals) | !params$time$time_window) {
    intervals = list("Full" = c(0, 0))
  }

  # For each interval, run the interval analysis
  interval_results <- purrr::imap(intervals, function(interval_range, interval_name) {

    interval_res <- analyze_interval(
      file_ctx      = file_ctx,
      interval_name = interval_name,
      interval_range = interval_range,
      params        = params
    )

    # add interval info
    interval_res$channels$summary <- dplyr::mutate(
      interval_res$channels$summary,
      interval = interval_name,
      interval_range = paste0(interval_range[1], " - ",
                              interval_range[2]),
      .before = 1)

    # Logging
    message("")
    # --- Save interval-level outputs ---
    # TODO movethis into an ad-hoc function
    # For each channel, save results (e.g., period_tbl, traces, etc.)
    purrr::walk(
      interval_res$channels[setdiff(names(interval_res$channels),
                                    "summary")],
      function(channel_res) {
      ch_id <- channel_res$ch_id

      # Save period table
      saveRDS(channel_res$period_table,
              file = file.path(
                rds_folder,
                paste0(
                  file_ctx$file_id, "_", interval_name, "_",
                  ch_id, "_period_tbl.rds"
                )
              )
      )

      # Save period table without NA
      saveRDS(channel_res$period_table[complete.cases(channel_res$period_table),],
              file = file.path(
                rds_folder,
                paste0(
                  file_ctx$file_id, "_", interval_name, "_",
                  ch_id, "_period_tbl_clean.rds"
                )
              )
      )

      # Save detrended traces
      saveRDS(channel_res$detrended_traces,
              file = file.path(
                rds_folder,
                paste0(
                  file_ctx$file_id, "_", interval_name, "_",
                  ch_id, "_detrended_traces.rds"
                )
              )
      )


      # Save cell traces
      saveRDS(channel_res$cells,
              file = file.path(
                rds_folder,
                paste0(
                  file_ctx$file_id, "_", interval_name, "_",
                  ch_id, "_cells.rds"
                )
              )
      )

      # Save AUC table
      saveRDS(channel_res$auc_results,
              file = file.path(
                rds_folder,
                paste0(
                  file_ctx$file_id, "_", interval_name, "_",
                  ch_id, "_auc_results.rds"
                )
              )
      )

      # Save network analysis
      if(params$analysis$network && !is.null(channel_res$network_results)){
        saveRDS(channel_res$network_results,
                file = file.path(
                  rds_folder,
                  paste0(
                    file_ctx$file_id, "_", interval_name, "_",
                    ch_id, "_network_results.rds"
                  )
                )
        )
      }

      # Save other outputs as needed

      # export file paths

    })

    interval_res # Return interval result for aggregation
  })

  summary <- purrr::imap(interval_results, function(interval, id) {
    dplyr::mutate(interval$channels$summary, file_id = file_ctx$file_id, .before = 1)
  }) |>
    purrr::list_rbind()

  # save file summary as csv and rds
  saveRDS(summary,
          file = file.path(
            rds_folder,
            paste0(
              file_ctx$file_id, "_period_table_summary.rds"
            )
          )
  )

  write.csv(
    summary,
    file = file.path(
      rds_folder,
      paste0(
        file_ctx$file_id, "_period_table_summary.csv"
      )
    )
  )

  # TODO Aggregate summaries from all intervals/channels
  # summaries <- purrr::map(interval_results, function(int_res) {
  #   purrr::map(int_res$channels, "summary")
  # }) |> purrr::flatten_df()

  # Return results for this file
  list(
    file_id    = file_ctx$file_id,
    filename   = file_ctx$filename,
    intervals  = interval_results,
    summaries = summary
  )
}

# Interval layer ####

#' analyze_interval
#'
#' Subsets channel data to a time interval and runs per-channel analysis,
#' returning results aggregated across all enabled channels.
#'
#' @param file_ctx A named list of file context data as produced by
#'   \code{prepare_file()}.
#' @param interval_name A character string naming the interval (e.g.
#'   \code{"baseline"}).
#' @param interval_range A numeric vector of length 2 giving the start and
#'   end of the interval in the same units as \code{params$time$time_res}.
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#'
#' @return A named list with elements \code{interval_name} (character),
#'   \code{interval_range} (numeric vector), and \code{channels} (a named
#'   list of per-channel results including a combined \code{summary} data frame).
#' @export
#'
#' @examples
#' \dontrun{
#' interval_result <- analyze_interval(file_ctx, "baseline", c(0, 24), params)
#' }
analyze_interval <- function(file_ctx, interval_name, interval_range, params) {

  # Compute interval indices
  if(params$time$time_window){
  interval_idx <- seq(from = interval_range[1] / params$time$time_res,
                      to   = interval_range[2] / params$time$time_res)
  } else {
    interval_idx <- NULL
  }

  # Log interval being analyzed
  message(paste0("      |--- Processing interval: ", interval_name ))

  # Subset each channel's data for the interval

  channels_subset <- purrr::imap(file_ctx$channels, function(channel_ctx,
                                                             ch_name) {

    # Get channel-specific parameters (e.g., invert flag)
    channel_params <- params$channels[[ch_name]]

    # Subset grid values to interval
    if(!is.null(interval_idx)){
    grid_vals_sub <- channel_ctx$grid_vals[, interval_idx, drop = FALSE]
    } else {
      grid_vals_sub <- channel_ctx$grid_vals
      interval_range <- c(0, dim(grid_vals_sub)[2]*params$time$time_res)
    }

    # filter cells within ROI
    within_scn <- sf::st_within(file_ctx$grid_points_sf,
                                file_ctx$scn_roi,
                                sparse = FALSE)
    grid_vals_sub <- grid_vals_sub[within_scn, , drop = FALSE]

    invert_ch <- ifelse(!is.null(channel_params$invert),
                        channel_params$invert,
                        FALSE
                        )

    # Build channel_ctx for this interval
    list(
      grid_vals      = grid_vals_sub,
      grid_points_sf = file_ctx$grid_points_sf,
      scn_roi        = file_ctx$scn_roi,
      grid_coord     = file_ctx$grid_coord,
      invert         = invert_ch,
      # Add other channel-specific metadata as needed
      ch_label       = channel_params$label
    )
  })

  # Analyze all enabled channels for this interval
  channel_results <- purrr::imap(channels_subset, function(channel_ctx,
                                                           ch_name) {

    # Only analyze if channel is enabled
    if (!params$channels[[ch_name]]$enabled) return(NULL)

    analyze_channel(
      channel_name = ch_name,
      channel_ctx  = channel_ctx,
      file_ctx     = file_ctx,
      params       = params
    )
  }) |> purrr::compact() # Remove NULLs for disabled channels

  channel_results$summary <-
    purrr::imap(channel_results, function(channel, id) {
      channel$period_summary |>
        dplyr::mutate(
          dplyr::across(
            where(~ inherits(.x, "circular")),
            as.numeric
          ),
          channel_id = id,
          .before = 1
        )
  }) |>
    purrr::list_rbind()

  # TODO Summarize results for the interval
  # aggregate summaries from each channel, e.g.:
  # summaries <- purrr::map(channel_results, "summary")

  # TODO adjust interval range output

  # Return results for this interval
  list(
    interval_name  = interval_name,
    interval_range = interval_range,
    channels       = channel_results
    # Optionally: summaries = summaries
    # list of saved objects (period_tbl, detrended_traces, channel_cells)
  )
}

' not needed now
    prepare_channel_data <- function(grid_vals, grid_points_sf, scn_roi, interval_idx) {
      # Subset grid values to interval
      grid_vals_sub <- grid_vals[, interval_idx, drop = FALSE]
      # Filter cells inside ROI
      within_scn <- sf::st_within(grid_points_sf, scn_roi, sparse = FALSE)
      grid_vals_sub[within_scn, , drop = FALSE]
    }
    '

# Channel layer ####

#' analyze_channel
#'
#' Runs the full analysis pipeline for a single channel within a given
#' interval, including period estimation, coherence metrics, AUC, network
#' analysis, and summary statistics.
#'
#' @param channel_name A character string identifying the channel (e.g.
#'   \code{"Ch1"}).
#' @param channel_ctx A named list of channel context data containing
#'   \code{grid_vals} (matrix of cell fluorescence traces, cells × frames),
#'   \code{grid_points_sf} (sf object of grid points), \code{scn_roi}
#'   (sf object), \code{grid_coord} (data frame of grid coordinates),
#'   \code{invert} (logical), and \code{ch_label} (character).
#' @param file_ctx A named list of file context data as produced by
#'   \code{prepare_file()}.
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#'
#' @return A named list with elements \code{ch_id}, \code{ch_label},
#'   \code{cells} (input grid values matrix), \code{period_table},
#'   \code{auc_results}, \code{network_results}, \code{circular_stats},
#'   \code{period_summary} (data frame), \code{detrended_traces},
#'   \code{mut_inf}, and \code{status}.
#' @export
#'
#' @examples
#' \dontrun{
#' ch_result <- analyze_channel("Ch1", channel_ctx, file_ctx, params)
#' }
analyze_channel <- function(channel_name, channel_ctx, file_ctx, params) {

  # TODO change cachepaths$period to actual value and uncomment
  '
      if(!params$reload && file.exists(cache_paths$period)) {
        list(
          period_tbl = readRDS(cache_paths$period),
          traces = readRDS(cache_paths$traces),
          results = readRDS(cache_paths$results)
        )
      } else {
      '

  # Log channel analysis
  message(paste0("           |--- Processing channel: ", channel_name ))

  period_res <- computePeriod(
    channel_ctx$grid_vals,
    file_ctx$filename,
    excludeNC = TRUE,
    top = 36,
    bottom = 16,
    time_res = params$time$time_res,
    invert = channel_ctx$invert,
    filterRAE = 0.90)

  # compute coherence parameters
  if (params$analysis$coherence) {
    coh <- add_coherence_metrics(period_tbl = period_res$period_table,
                                 grid_coord = channel_ctx$grid_coord
    )

    period_res$period_table <- coh$period_table
    mi <- coh$mi
  } else {
    period_res$period_table <- period_res$period_table
    mi <- NA
  }

  # Calculate AUC

    auc_results <- data.frame(ID = as.integer(rownames(channel_ctx$grid_vals)),
                          AUC = as.data.frame(
                            apply(channel_ctx$grid_vals, 1, calculate_auc)
                            )
                          ) %>% `colnames<-`(c("ID", "AUC"))

  # TODO wrap into a function
  if(FALSE){
  nonrhythmic_cells <- setdiff(rownames(channel_ctx$grid_vals), period_res$period_table$ID)
  cell_rows_nonrhythmic <- which(rownames(channel_ctx$grid_vals) %in% nonrhythmic_cells)
  cell_vals_nonrhythmic <- channel_ctx$grid_vals[cell_rows_nonrhythmic, ]
  suppressMessages(
  period_res_out <- computePeriod(
         cell_vals_nonrhythmic,
         file_ctx$filename,
         excludeNC = TRUE,
         top = 96,
         bottom = 4,
         time_res = params$time$time_res,
         invert = channel_ctx$invert)
  )
  }

  # Calculate mean trace and relative period
  period_mean_trace <- mean_trace_period(channel_ctx,
                                         file_ctx$file_id,
                                         params$time$time_res)

  # Calculate circular stats
  circular_stats <- circular_stats(period_res$period_table)

  status = NULL #TODO add status after running tests

  ch_label = params$channels[[channel_name]]$label

  # perform network analysis
  if(isTRUE(params$analysis$network) && nrow(period_res$period_table) > 0){

  network_results <- network_analysis(filename = "placeholder",
                                      interval_name = "placeholder",
                                      cell_traces = channel_ctx$grid_vals,
                                      grid_coord = channel_ctx$grid_coord,
                                      period_table = period_res$period_table,
                                      network_dir = "placeholder")
  }else{
    network_results <- NULL
  }
  # generate period summary table
  period_summary_table <- summarizePeriod(
    period_res,
    # filename = file_ctx$file_id,
    circ_stats = circular_stats,
    # TODO get info and add interval_name = ... ,
    Ch_rep = ch_label,
    coherence = params$analysis$coherence) |>
    mutate(period_mean_trace = period_mean_trace$period) |>
    as.data.frame()


  # Returns a list/tibble of results for this channel and interval
  list(
    ch_id = channel_name,
    ch_label = ch_label,
    cells = channel_ctx$grid_vals,
    period_table = period_res$period_table,
    auc_results = auc_results,
    network_results = network_results,
    circular_stats = circular_stats,
    period_summary = period_summary_table,
    detrended_traces = period_res$traces,
    mut_inf = mi,
    status = status
  )
}

#' add_coherence_metrics
#'
#' Computes spatial coherence scores for period, phase, and amplitude, and
#' calculates normalised mutual information between period and phase coherence.
#'
#' @param period_tbl A data frame of per-cell period analysis results
#'   containing at minimum columns \code{ID}, \code{period},
#'   \code{phase_circ}, and \code{amplitude}.
#' @param grid_coord A data frame of grid coordinates used to define spatial
#'   neighbourhoods for coherence calculation.
#'
#' @return A named list with elements \code{period_table} (the input data
#'   frame augmented with columns \code{period_crn}, \code{amp_crn}, and
#'   \code{phase_crn}) and \code{mi} (a numeric scalar for normalised mutual
#'   information between period and phase coherence).
#' @export
#'
#' @examples
#' \dontrun{
#' coh <- add_coherence_metrics(period_tbl, grid_coord)
#' }
add_coherence_metrics <- function(period_tbl, grid_coord){

# TODO resolve inconsistency between period_tbl and period_table
  period_crn <- coherence_analysis(
    period_tbl, grid_coord, radius = 100,
    variable = "period", threshold = 0.1, merge = FALSE
  ) |> `colnames<-`(c("ID", "period_crn"))

  phase_crn <- coherence_analysis(
    period_tbl, grid_coord, radius = 100,
    variable = "phase_circ", threshold = 0.1, merge = FALSE
  ) |> `colnames<-`(c("ID", "phase_crn"))

  amp_crn <- coherence_analysis(
    period_tbl, grid_coord, radius = 100,
    variable = "amplitude", threshold = 0.1, merge = FALSE
  ) |> `colnames<-`(c("ID", "amp_crn"))

  # TODO add option to add similarity score and composite score

  list(
    period_table = plyr::join_all(
      list(period_tbl, period_crn, amp_crn, phase_crn),
      by = "ID", type = "left"
    ),
    mi = mutual_information(period_crn$period_crn, phase_crn$phase_crn,
                            n_bins = 10, normalize = TRUE)
  )
}

# Plot generation ####

#' generate_plots
#'
#' Generates and saves all configured plot types for each file, interval, and
#' channel, including network plots if enabled.
#'
#' @param file_rows A tibble of file metadata as produced by \code{index_files()}.
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#' @param ... Additional arguments passed to plotting functions. Currently
#'   supports \code{p.width} (integer, plot width in pixels).
#'
#' @return Called for its side effects (plot files written to disk). Returns
#'   \code{NULL} invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_plots(file_rows, params, p.width = 1600)
#' }
generate_plots <- function(file_rows, params, ...) {
  plot_ranges <- params$plotting$ranges
  args <- list(...)

  p.width <- args$p.width

  n_files <- nrow(file_rows)
  pb_id <- cli::cli_progress_bar("Generating plots", total = n_files)

  purrr::pmap(
    file_rows,
    function(file_id, file_path, folder_path, ...) {

      file_row <- list(file_id = file_id,
                       file_path = file_path,
                       folder_path = folder_path)

      file_ctx <- prepare_file(file_row = file_row,
                               params = params)
      # Create plots folder for this file
      # TODO check folder structure created
      plots_folder <- file.path(file_ctx$folder_path, "plots")
      if (!dir.exists(plots_folder)) dir.create(plots_folder, recursive = TRUE)

      purrr::walk(
        names(params$time$intervals),
        function(interval_name) {

        interval_range <- params$time$intervals[[interval_name]]

        # Pre-load all files
        channel_data_list <- purrr::map(
          names(params$channels),
          function(ch_id) {
            if (!params$channels[[ch_id]]$enabled) return(NULL)

            # list of objects to be passed outside
            ch_data <- list(
              ch_id = ch_id,
              cells = file.path(file_row$folder_path, "rds",
                                paste0(
                                  file_row$file_id, "_",
                                  interval_name, "_",
                                  ch_id, "_cells.rds"
                                )
              ) |> readRDS(),
              period_tbl = file.path(file_row$folder_path, "rds",
                                     paste0(
                                       file_row$file_id, "_",
                                       interval_name, "_",
                                       ch_id, "_period_tbl.rds"
                                     )
              ) |> readRDS(),
              auc_results = file.path(file_row$folder_path, "rds",
                                      paste0(
                                        file_row$file_id, "_",
                                        interval_name, "_",
                                        ch_id, "_auc_results.rds"
                                      )
              ) |> readRDS(),
              network = NULL,
              ranges = plot_ranges[[ch_id]],
              cell_coords = file_ctx$grid_coord,
              summary_results = file.path(file_row$folder_path, "rds",
                                          paste0(
                                            file_row$file_id,
                                            "_period_table_summary.rds"
                                          )
              ) |> readRDS()
            )



            network_res_path <- file.path(file_row$folder_path, "rds",
                                          paste0(
                                            file_row$file_id, "_",
                                            interval_name, "_",
                                            ch_id, "_network_results.rds"
                                          )
            )
            if(params$analysis$network && file.exists(network_res_path)){
              ch_data$network <- readRDS(network_res_path)
            }

            return(ch_data)
          })
        names(channel_data_list) <- names(params$channels)
        channel_data_list <- channel_data_list |> purrr::compact()

        # pass the relevant data to a walk function to generate and save the
        # single channel plots


        # make a different function to save the multi-channel plots

        purrr::walk(
          names(channel_data_list),
          function(ch_id) {

            ch_data <- channel_data_list[[ch_id]]

            # Create channel folder - add interval layer only if multiple intervals
            if (length(params$time$intervals) > 1) {
              interval_folder <- file.path(plots_folder, interval_name)
              if (!dir.exists(interval_folder)) dir.create(interval_folder, recursive = TRUE)
              ch_id_folder <- file.path(interval_folder, ch_id)
            } else {
              ch_id_folder <- file.path(plots_folder, ch_id)
            }

          if (!dir.exists(ch_id_folder)) dir.create(ch_id_folder, recursive = TRUE)


          # cells <- readRDS(cell_path)
          # period_tbl <- readRDS(period_tbl_path)
          # auc_results <- readRDS(auc_res_path)
          # ranges <- plot_ranges[[interval_name]]
          # cell_coords <- file_ctx$grid_coord

          p.width <- ifelse(is.null(p.width), 1600, p.width)

          # For each plot type generate plot
          purrr::walk(params$plotting$plot_types, function(plot_type) {

            # Create plot object using the assigned function
            plot_obj <- create_plot(plot_type,
                                    period_tbl = ch_data$period_tbl,
                                    auc_results = ch_data$auc_results,
                                    summary_results = ch_data$summary_results,
                                    cell_coords = ch_data$cell_coords,
                                    cells = ch_data$cells,
                                    ranges = ch_data$ranges,
                                    plotting_params = params$plotting,
                                    file_row = file_row,
                                    interval_name = interval_name,
                                    ch_id = ch_id
            )

            # Save plot
            # TODO adjust saving parameters and path creation
            if (is.na(plot_obj$status)) {

            } else if (plot_obj$status) {

              # calculate plot height
              p.height = plot_height(plot_type,
                                     plot_width = p.width,
                                     cell_coords = ch_data$cell_coords)

              save_path <- build_plot_path(file_id = file_row$file_id,
                                           ch_id = ch_id,
                                           ch_id_folder = ch_id_folder,
                                           plot_name = plot_obj$plot_name,
                                           save_format = params$plotting$save_format)

              save_plot(plot_obj$plot,
                        save_path,
                        params$plotting$save_format,
                        p.width = p.width,
                        p.height = p.height
              )
            } else {
              message(paste0(" Could not create ", plot_obj$plot_name))
            }

          })

          if(params$analysis$network){ # TODO change and import earlier

            # import network variables

            network_res_path <- file.path(file_row$folder_path, "rds",
                                          paste0(
                                            file_row$file_id, "_",
                                            interval_name, "_",
                                            ch_id, "_network_results.rds"
                                          )
            )
            if(file.exists(network_res_path)){
              network_res <- readRDS(network_res_path)

              # generate network plots
              purrr::walk(params$plotting$network_plot_types, function(plot_type) {

                plot_obj <- create_plot(plot_type, # check required inputs
                                        network_results = network_res,
                                        cell_coords = ch_data$cell_coords,
                                        ranges = ch_data$ranges,
                                        plotting_params = params$plotting,
                                        file_row = file_row,
                                        interval_name = interval_name,
                                        ch_id = ch_id
                )

                # Save plot
                # TODO adjust saving parameters and path creation
                if (is.na(plot_obj$status)) {

                } else if (plot_obj$status) {

                  # calculate plot height
                  p.height = plot_height(plot_type, plot_width = p.width,
                                         node_data = network_res$node_data,
                                         cell_coords = ch_data$cell_coords)

                  save_path <- build_plot_path(file_id = file_row$file_id,
                                               ch_id = ch_id,
                                               ch_id_folder = ch_id_folder,
                                               plot_name = plot_obj$plot_name,
                                               save_format = params$plotting$save_format)

                  save_plot(plot_obj$plot,
                            save_path,
                            params$plotting$save_format,
                            p.width = p.width,
                            p.height = p.height
                  )
                } else {
                  message(paste0(" Could not create ", plot_obj$plot_name))
                }
              })
            }
          }

        })
        # TODO add here a call for plot involving multiple channels
        # browser()
        # check list of multiple

      })
      cli::cli_progress_update(id = pb_id)
    })
  cli::cli_progress_done(id = pb_id)
  message("Plots generated.")
}


#' make_params
#'
#' Constructs and validates the parameter list used throughout the analysis
#' and plotting pipeline, applying defaults for any unspecified options.
#'
#' @param channels A named list of channel definitions. Each element should
#'   contain at minimum \code{enabled} (logical), \code{label} (character),
#'   \code{invert} (logical), and \code{grid_file} (character).
#' @param coherence Logical; whether to compute spatial coherence metrics.
#'   Defaults to \code{TRUE}.
#' @param normalize_phase Character string naming the channel to use as phase
#'   reference for normalisation, or \code{NULL} to skip. Defaults to
#'   \code{NULL}.
#' @param time_res Numeric; the temporal resolution of the data in minutes per
#'   frame.
#' @param intervals A named list of numeric vectors of length 2 defining time
#'   intervals (start and end in the same units as \code{time_res}), or
#'   \code{NULL}.
#' @param time_window Logical; whether to subset data to defined intervals.
#'   Defaults to \code{FALSE}.
#' @param pixel_fct Numeric; scaling factor converting pixel units to physical
#'   units.
#' @param network Logical; whether to perform network analysis. Defaults to
#'   \code{TRUE}.
#' @param saving Logical; whether to save outputs to disk. Defaults to
#'   \code{TRUE}.
#' @param reload_period Logical; whether to reload previously computed period
#'   results from cache. Defaults to \code{FALSE}.
#' @param plotting A named list of plotting options. See Details.
#' @param folder_structure A named list with element \code{base_dir}
#'   specifying the root output directory.
#'
#' @return A named list of validated analysis parameters, with defaults
#'   applied for any unspecified plotting options.
#' @export
#'
#' @examples
#' \dontrun{
#' params <- make_params(
#'   channels = list(Ch1 = list(enabled = TRUE, label = "RCaMP",
#'                              invert = FALSE, grid_file = "ch1.csv")),
#'   time_res = 30,
#'   pixel_fct = 0.8
#' )
#' }
make_params <- function(
    channels = list(),
    coherence = TRUE,
    network = TRUE,
    normalize_phase = NULL,
    time_res,
    intervals = NULL,
    time_window = FALSE,
    pixel_fct,
    saving = TRUE,
    reload_period = FALSE,
    plotting = list(
      y_limits_from_previous = TRUE,
      previous_exp = "path/to/previous/exp",
      override_ranges = FALSE,
      ranges = list(),  # Will be filled by calculate_plot_ranges
      plot_types = c("period",
                     "amplitude",
                     "RAE",
                     "AUC",
                     "phase",
                     "phase_norm",
                     "3D"),  # List of plots to generate
      save_format = "svg",
      #threeD = TRUE,
      #phase_plots = TRUE,
      colors = list(
        Ch1 = "darkgreen",
        Ch2 = "purple",
        Ch3 = "#e5e372",
        Ch4 = "#00aefe"
        #... add more if needed
      )
    ),
    folder_structure = list(
      base_dir = "results",
      interval_dir = "interval",
      channel_dir = "channel"
    )
) {

 # TODO add a way that default parameters are automatically added if not
 # declared. i.e. plotting is a list by default but user building its own list
 # may override default undeclared settings

  params_list <-
    list(
    channels = channels,
    analysis = list(
      coherence = coherence,
      network = network,
      normalize_phase = normalize_phase
    ),
    time = list(
      time_res = time_res,
      intervals = intervals,
      time_window = time_window
    ),
    pixel_fct = pixel_fct,
    saving = saving,
    reload_period = reload_period,
    plotting = plotting,
    paths = folder_structure
    # TODO add option when interval_dir and Channel_dir are not declared
  )

  # add some additional parameters if undeclared
  # saving format
  if (is.null(params_list$plotting$save_format)) {
    params_list$plotting$save_format <- "svg" # default to svg
  }

  if (is.null(params_list$plotting$plot_types)) {
    plot_types = c("period",
                   "period_hist",
                   "amplitude",
                   "error",
                   "RAE",
                   "AUC",
                   "phase",
                   "phase_norm",
                   "phase_circ",
                   "period_crn",
                   "amplitude_crn",
                   "phase_crn",
                   "phase_circular",
                   "phase_distribution"
    )

    params_list$plotting$plot_types <- plot_types

  }

  if (is.null(params_list$plotting$network_plot_types) && network) {
    network_plots = c("degree_hist",
                      "strength_hist",
                      "degree_weight_hist",
                      "spatial_node_weights",
                      "spatial_node_strengths",
                      "spatial_node_clust_coeff",
                      "spatial_clusters",
                      "clusters_phases",
                      "clusters_period",
                      "clusters_amplitude",
                      "clusters_per_crn",
                      "cluster_hist",
                      "clusters_doughnut",
                      # "overlap_hetmap",
                      "corr_plot"
                      )  # List of network plots to generate

    params_list$plotting$network_plot_types <- network_plots

  }

  # Default colors
  if (is.null(params_list$plotting$colors)) {
    params_list$plotting$colors <- list(
      Ch1 = "darkgreen",
      Ch2 = "purple",
      Ch3 = "#e5e372",
      Ch4 = "#00aefe"
    )
  }
  # Add ranges object
  if(is.null(params_list$plotting$ranges)) {
    params_list$plotting$ranges <- list()
  }

  # Add fetching of ranges from previous experiment
  if (params_list$plotting$y_limits_from_previous &&
      !is.null(params_list$plotting$previous_exp)
  ) {
    # assign parameters
  }


  # TODO verify and uncomment validate_params call
  # validate_params(params_list)

  return(params_list)
}

#' validate_params
#'
#' Checks a parameter list for completeness and internal consistency, stopping
#' with an informative error if any required fields are missing or invalid.
#'
#' @param p A named list of analysis parameters as produced by \code{make_params()}.
#'
#' @return Called for its side effects. Returns \code{TRUE} invisibly if all
#'   checks pass.
#'
#' @examples
#' \dontrun{
#' validate_params(params)
#' }
validate_params <- function(p) {
  stopifnot(is.list(p))

  ## ---- Channels ----
  channels <- p$channels
  if (length(channels) == 0)
    stop("No channels defined.")

  enabled <- vapply(channels, '[[', logical(1), "enabled")
  if (!any(enabled))
    stop("At least one channel must be enabled.")

  required_fields <- c("enabled", "label", "invert", "grid_file")
  for (nm in names(channels)) {
    missing <- setdiff(required_fields, names(channels[[nm]]))
    if (length(missing) > 0)
      stop("channel ", nm, " missing fields: ", paste(missing, collapse = ", "))
  }

  ## ---- Normalize phase ----
  # TODO add check that only one channel is used for normalization
  norm <- p$analysis$normalize_phase
  if (!is.null(norm)) {
    if (!norm %in% names(channels))
      stop("normalize_phase refers to unknown channel: ", norm)
    if (!channels[[norm]]$enabled)
      stop("normalize_phase channel is not enabled: ", norm)
  }

  ## ---- Time ----

  if (p$time$time_res <= 0)
    stop("time_res must be > 0")

  if (p$time$time_window) {
    ints <- p$time$intervals
    if (is.null(ints) || length(ints) == 0)
      stop("time_window = TRUE but no intervals provided")

    for (nm in names(ints)) {
      int <- ints[[nm]]
      if (!is.numeric(int) || length(int) != 2 || int[1] >= int[2])
        stop("Invalid interval: ", nm)
    }
  }

  ## ---- Plotting ----
  plot <- p$plotting
  if (plot$y_limits_from_previous && is.null(plot$previous_exp))
    stop("previous_exp must be provided when y_limits_from_previous = TRUE")

  ## --- Filepaths ----
  # TODO add check for length of longest filepath (3D plot generation if
  # active and alert user that saving will fail if left unchanged)

  invisible(TRUE)
}

#' generate_reports
#'
#' Renders a per-file report for each file in the project, collecting all plot
#' types across intervals and channels. Output is HTML by default (no system
#' dependencies); set \code{pdf = TRUE} for PDF output (requires additional
#' system dependencies — see Details).
#'
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#' @param file_rows A tibble of file metadata as produced by \code{index_files()}.
#' @param pdf Logical; if \code{TRUE} render PDF reports instead of HTML.
#'   Requires \pkg{magick}, \pkg{tinytex}, and a TinyTeX installation
#'   (\code{tinytex::install_tinytex()}). Defaults to \code{FALSE}.
#'
#' @return Called for its side effects (report files written to disk). Returns
#'   \code{NULL} invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_reports(params, file_rows)
#' generate_reports(params, file_rows, pdf = TRUE)
#' }
generate_reports <- function(params, file_rows, pdf = FALSE) {
  intervals   <- names(params$time$intervals)
  channels    <- names(params$channels)
  plot_types  <- c(params$plotting$plot_types, params$plotting$network_plot_types)
  base_dir    <- params$paths$base_dir
  reports_dir <- file.path(base_dir, "reports")

  if (!dir.exists(reports_dir)) dir.create(reports_dir)

  n_files <- length(file_rows$file_id)
  pb_id   <- cli::cli_progress_bar("Generating reports", total = n_files)

  purrr::walk(file_rows$file_id, function(fid) {
    generate_file_report(
      file_id    = fid,
      params     = params,
      base_dir   = base_dir,
      intervals  = intervals,
      channels   = channels,
      plot_types = plot_types,
      pdf        = pdf
    )
    cli::cli_progress_update(id = pb_id)
  })

  cli::cli_progress_done(id = pb_id)
  message("Reports generated.")
}

#' generate_plot_type_reports
#'
#' Renders one report per plot type, collating the corresponding plot across
#' all files, intervals, and channels using an Rmd template. Output is HTML
#' by default; set \code{pdf = TRUE} for PDF output (requires additional
#' system dependencies — see Details).
#'
#' @param params A named list of analysis parameters as produced by \code{make_params()}.
#' @param file_rows A tibble of file metadata as produced by \code{index_files()}.
#' @param storage_fold Character string naming the subfolder within each
#'   file's results directory where plots are stored. Defaults to
#'   \code{"plots"}.
#' @param plot_report_dir Character string giving the output directory for
#'   rendered reports. Defaults to \code{file.path(params$paths$base_dir,
#'   "reports", "plot_reports")}.
#' @param pdf Logical; if \code{TRUE} render PDF reports instead of HTML.
#'   Requires \pkg{magick}, \pkg{tinytex}, and a TinyTeX installation
#'   (\code{tinytex::install_tinytex()}). Defaults to \code{FALSE}.
#'
#' @return Called for its side effects (reports written to
#'   \code{plot_report_dir}). Returns \code{NULL} invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_plot_type_reports(params, file_rows)
#' generate_plot_type_reports(params, file_rows, pdf = TRUE)
#' }
generate_plot_type_reports <- function(
    params,
    file_rows,
    storage_fold    = "plots",
    plot_report_dir = file.path(params$paths$base_dir, "reports", "plot_reports"),
    pdf             = FALSE
) {
  if (pdf) {
    check_pdf_deps()
    plot_report_template <- "plot_type_report_template_pdf.Rmd"
    output_ext           <- ".pdf"
  } else {
    plot_report_template <- "plot_type_report_template.Rmd"
    output_ext           <- ".html"
  }

  intervals  <- names(params$time$intervals)
  channels   <- names(params$channels)
  plot_types <- c(params$plotting$plot_types, params$plotting$network_plot_types)
  base_dir   <- params$paths$base_dir

  if (!dir.exists(plot_report_dir)) dir.create(plot_report_dir, recursive = TRUE)

  n_plot_types <- length(plot_types)
  pb_id        <- cli::cli_progress_bar("Generating plot-type reports", total = n_plot_types)

  purrr::walk(plot_types, function(pt) {
    rmarkdown::render(
      input       = system.file("rmd", plot_report_template, package = "ClockCyteR.spatial"),
      output_file = file.path(plot_report_dir, paste0("plot_report_", pt, output_ext)),
      params      = list(
        channel_params = params$channels,
        file_rows      = file_rows,
        base_dir       = base_dir,
        intervals      = intervals,
        channels       = channels,
        plot_type      = pt,
        save_format    = params$plotting$save_format
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )

    cli::cli_progress_update(id = pb_id)
  })

  cli::cli_progress_done(id = pb_id)
  message("Plot-type reports generated in: ", plot_report_dir)
}


