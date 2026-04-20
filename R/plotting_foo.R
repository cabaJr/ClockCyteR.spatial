# collection of plotting functions

# create circular plot
#' Circular phase plot with optional mean-phase alignment
#'
#' @param data Numeric vector of phases in radians.
#' @param path Character; file path where the SVG is saved when
#'   \code{saving = TRUE}.
#' @param saving Logical; whether to write the plot to file. Defaults to
#'   \code{TRUE}.
#' @param align Logical; whether to rotate the distribution so the mean phase
#'   falls at \code{align_to}. Defaults to \code{TRUE}.
#' @param align_to Numeric; target hour for the mean-phase alignment.
#'   Defaults to \code{6}.
#' @param inside_plot Logical; reserved for future use. Defaults to
#'   \code{TRUE}.
#' @param pt_size Numeric; point size for individual observations. Defaults to
#'   \code{0.30}.
#' @param pt_col Character; color for individual observation points. Defaults
#'   to \code{"#eb0000"}.
#' @param ... Optional arguments passed to downstream calls; supports
#'   \code{plot_title} (character title shown above the plot).
#'
#' @return Invisibly \code{NULL}; the plot is written to file when
#'   \code{saving = TRUE}.
#' @export
circular_plot <- function(data,
                         path,
                         saving = TRUE,
                         align = TRUE,
                         align_to = 6,
                         inside_plot = TRUE,
                         pt_size = 0.30,
                         pt_col = '#eb0000',
                         ...
                         ){

  phases_rad = data
  phases_rad <- phases_rad[!is.na(phases_rad)]
  args <- list(...)
  plot_title <- args$plot_title
  if(!all(is.na(phases_rad)) && (length(phases_rad) > 0)){
    real_MeanPh <- circular::mean.circular(phases_rad,
                                           Rotation = "counter",
                                           na.rm = TRUE,
                                           type = "angles",
                                           units = "radians",
                                           template = "none",
                                           modulo = "asis",
                                           zero = 0)
    #phases_rad = align_phase(phases_rad, CT = 6)
    if(align){
      phases_rad = phases_rad + ((align_to/24)*2*pi) - real_MeanPh
      # message("aligned to ", align_to, "h")
    }
    phases = circular::circular(phases_rad,
                                units = "radians",
                                template = "clock24"
                                )
  }
  if(saving){
    svgdatapath = path

    svglite::svglite(
      filename = svgdatapath,
      width = 5,
      height = 4.8,
      # family = "sans",
      bg = "transparent"
      )
  }
  # if there are no data to plot
  if(suppressWarnings(is.infinite(max(phases_rad, na.rm = TRUE)))){
    phases = NA
    real_MeanPh = NA
    plot.new()

    suppressWarnings(
    circular::plot.circular(phases,
                            bins = 24,
                            axes = FALSE,
                            type = "angles",
                            units = "radians",
                            rotation = "clock",
                            template = "none",
                            modulo = "asis",
                            zero = 0
                            )
    )

    suppressWarnings(
    circular::axis.circular(circular(seq(0, 2*pi, pi/2)),
                            zero=0,
                            rotation = 'counter',
                            labels=c("6", "00", "18", "12", "6"),
                            tcl.text=0.12,
                            cex = 1.5
                            )
    )
    title(paste(plot_title), line = 0)
    dev.off()
    variance = NA
    vectorLength = NA
    RT = NA
  }else if(inside_plot){
    phases_rad <- as.numeric(phases)

    # Sort the phase values to process in a consistent order
    sorted_indices <- order(phases_rad)
    phases_sorted <- phases_rad[sorted_indices]
    phases_sorted_correct <- phases_sorted

    # Initialize radii at 1 (edge of unit circle)
    radii_sorted <- rep(1, length(phases_sorted))
    min_sep <- 0.01  # angular distance threshold

    # reduce radius to cause plotting inside the circle
    radii_sorted = radii_sorted - 0.01
    # Loop through and reduce radius for close points
    for (i in 2:length(phases_sorted)) {
      # Check distance to previous point
      if (abs(phases_sorted[i] - phases_sorted[i - 1]) < min_sep) {
        radii_sorted[i] <- radii_sorted[i - 1] - runif(1, 8, 15)/1000
        phases_sorted_correct[i] <- phases_sorted[i] + runif(1, 0, 1)/15
        # Avoid going below a minimum radius
        if (radii_sorted[i] < 0.25) radii_sorted[i] <- 0.25
      }else{
        phases_sorted_correct[i] <- phases_sorted[i]
      }
    }

    # Convert polar to Cartesian coordinates
    x <- radii_sorted * sin(phases_sorted_correct)
    y <- radii_sorted * cos(phases_sorted_correct)

    # Prepare plot
    plot.new()

    circular::plot.circular(phases,
                            bins = 24,
                            axes = FALSE,
                            cex = 0,
                            shrink = 1.2
                            )

    # Plot jittered points
    points(x, y, cex = pt_size, pch = 21, bg = pt_col, col = NA)

    # Add radial axis labels
    angles <- seq(0, 2 * pi, length.out = 5)[-5]
    labels <- c("0", "6", "12", "18")
    label_radius <- 1.15

    # Add tick lines
    circular::axis.circular(circular(seq(0, 2*pi, pi/2)),
                            zero=0,
                            rotation = 'counter',
                            labels=rep("", 5),
                            tcl.text=0.12,
                            cex = 1.5
                            )

    # Add external labels
    text(label_radius * sin(angles),
         label_radius * cos(angles),
         labels = labels,
         cex = 1.2)

    # draw circle on top of points
    theta <- seq(0, 2 * pi, length.out = 500)
    lines(cos(theta), sin(theta), lwd = 1.2, col = "black")

    # Compute and plot mean direction
    meanTest <- circular::mean.circular(phases,
                                        Rotation = "counter",
                                        na.rm = TRUE
                                        )
    RT <- circular::rayleigh.test(phases)
    vectorLength <- RT$statistic[1]
    circStats <- round(RT$p.value[1], 3)

    circular::arrows.circular(meanTest,
                              vectorLength,
                              length = 0.1,
                              col = '#000000',
                              lwd = 3
                              )

    # Add title
    title(paste(plot_title), line = 0)

    # Angular variance
    variance <- circular::angular.variance(phases, na.rm = TRUE)

    # Close the plot
    dev.off()
  }else{
    #rotation = pi/2 - mean(phases)
    plot.new()
    # Create a circular plot
    circular::plot.circular(phases,
                            bins = 24,
                            axes = FALSE,
                            cex = 0)
    #, col = 'black', cex = 0, pch = 0, ticks = FALSE, tcl = 0)

    # Add points to the existing plot
    circular::points.circular(phases,
                              cex = pt_size,
                              pch = 21,
                              bg = pt_col,
                              col = NA)
    #, sep = 0.01)

    # Add radial axis labels to the plot
    angles <- seq(0, 2 * pi, length.out = 5)[-5]
    labels <- c("0", "6", "12", "18")
    label_radius <- 1.15

    # Add tick lines
    circular::axis.circular(circular(seq(0, 2*pi, pi/2)),
                            zero=0,
                            rotation = 'counter',
                            labels=c("", "", "", "", ""),
                            tcl.text=0.12,
                            cex = 1.5)

    # Add external labels
    text(label_radius * sin(angles),
         label_radius * cos(angles),
         labels = labels,
         cex = 1.2)

    # Calculate the mean circular statistic
    meanTest <- circular::mean.circular(phases,
                                        Rotation = "counter",
                                        na.rm = TRUE)

    # Perform a Rayleigh test on RCaMP
    RT <- rayleigh.test(phases)
    # Calculate the vector length statistic
    vectorLength <- RT$statistic[1]
    # Get the p-value from the Rayleigh test
    circStats <- RT$p.value[1]
    # Round the p-value to 3 decimal places
    circStats <- round(circStats, digits = 3)
    # Add arrows to the plot based on the mean and vector length
    circular::arrows.circular(meanTest,
                              vectorLength,
                              length = 0.1,
                              col = '#000000',
                              lwd = 3)

    # Add an empty title (the paste("") effectively removes the default title)
    title(paste(plot_title), line = 0)
    #calculate angular variance
    variance = circular::angular.variance(phases, na.rm = TRUE)
    # close plot
    dev.off()
  }

  list_returns <- list(#plot = plot,
    vec_len = vectorLength,
    variance = variance,
    meanP = real_MeanPh
    #RT = RT
  )

  return(list_returns)
}

# circular plot for different region of the sample
#' Circular stats plot with per-group mean arrows
#'
#' @param data Named list; each element is a numeric vector of phases in
#'   radians for one group.
#' @param path Character; file path where the SVG is saved.
#' @param colors Character vector of colors, one per group in \code{data}.
#'
#' @return Invisibly \code{NULL}; the plot is written to \code{path}.
#' @export
circStats_plot <- function(data, path, colors){
  data = data

  svgdatapath = path
  svg(filename = svgdatapath,
      width = 5,
      height = 4.8,
      family = "sans",
      bg = "transparent")

  plot.new()
  # Create a circular plot
  circular::plot.circular(NA,
                          bins = 24,
                          axes = FALSE,
                          type = "angles",
                          units = "radians",
                          rotation = "clock",
                          template = "none",
                          modulo = "asis",
                          zero = 0
                          )
  # Add radial axis labels to the plot
  circular::axis.circular(circular(seq(0, 2*pi, pi/2)),
                          zero=0,
                          rotation = 'counter',
                          labels=c("6", "00", "18", "12", "6"),
                          tcl.text=0.12,
                          cex = 1.5,
                          units = "radians",
                          template = "none",
                          modulo = "asis"
                          )
  # Add arrows to the plot based on the mean and vector length
  # close arrow
  vectorLength = data["close_vec_len"][[1]]
  meanTest = data["close_meanPh"][[1]]
  variance = data["close_variance"][[1]]
  color = colors[1]
  circular::arrows.circular(meanTest,
                            vectorLength,
                            length = 0.1,
                            col = color,
                            lwd = 3,
                            rotation = "clock"
                            )
  # mid arrow
  vectorLength = data["mid_vec_len"][[1]]
  meanTest = data["mid_meanPh"][[1]]
  color = colors[2]
  circular::arrows.circular(meanTest,
                            vectorLength,
                            length = 0.1,
                            col = color,
                            lwd = 3,
                            rotation = "clock"
                            )
  # far arrow
  vectorLength = data["far_vec_len"][[1]]
  meanTest = data["far_meanPh"][[1]]
  color = colors[3]
  circular::arrows.circular(meanTest,
                            vectorLength,
                            length = 0.1,
                            col = color,
                            lwd = 3,
                            rotation = "clock"
                            )

  # Add an empty title (the paste("") effectively removes the default title)
  title(paste(filename), line = 0)
  # close plot
  dev.off()
}

#' Jitter + boxplot of a variable stratified by cluster
#'
#' @param data Data frame containing the variables to plot.
#' @param x_var Character; column name to use on the x-axis (cluster
#'   grouping).
#' @param y_var Character; column name to use on the y-axis (numeric
#'   variable).
#' @param color_var Character; column name used for point and box color.
#' @param alpha_point Numeric; transparency of jittered points. Defaults to
#'   \code{0.3}.
#' @param alpha_box Numeric; transparency of the boxplot fill. Defaults to
#'   \code{0.2}.
#' @param colorscale Named character vector mapping cluster levels to colors.
#' @param x_label Character; x-axis label. Defaults to \code{"Cluster"}.
#' @param y_label Character; y-axis label. Defaults to \code{""}.
#' @param ylims Numeric vector of length 2 passed to \code{ylim()}; or
#'   \code{NULL} to use automatic limits. Defaults to \code{NULL}.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
cluster_plot <- function(data, x_var, y_var, color_var, alpha_point = 0.3, alpha_box = 0.2, colorscale, x_label = "Cluster", y_label = "", ylims = NULL){

  plot <- ggplot(data, aes(x = factor(!!sym(x_var)), y = as.numeric(!!sym(y_var)), color = !!sym(color_var))) +
    geom_point(alpha = alpha_point) +
    geom_boxplot(outlier.shape = NA, alpha = alpha_box, linewidth = 1) +
    scale_color_manual(values = colorscale, name = x_label) +
    labs(x = x_label, y = y_label) +
    scale_alpha_identity()+
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 1),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.title.y = element_text(size = 24),              # Increase y-axis title size
      axis.title.x = element_text(size = 24),              # Increase x-axis title size
      axis.text.y = element_text(size = 18),               # Increase y-axis label size
      axis.text.x = element_text(size = 18)
    )
  if(!is.null(ylims)){
    # TODO add checks on ylims being two numbers and good
    plot <- plot +
      ylims(ylims)
  }

  return(plot)
}

#' Dispatch a plot by type
#'
#' @param plot_type Character; key identifying which plot to produce (e.g.
#'   \code{"circular"}, \code{"period"}).
#' @param plotting_params List of plotting configuration parameters (ranges,
#'   color codes, etc.).
#' @param ... Additional arguments passed to the underlying plot function.
#'
#' @return A named list with elements \code{plot} (the \code{ggplot} or
#'   base-graphics object), \code{plot_name} (character), and \code{status}
#'   (character).
#' @export
create_plot <- function(plot_type, plotting_params, ...) {
  # need to receive plot type, data, and plotting params
  '
    plot_type,
    period_tbl = period_tbl,
    cells = cells,
    ranges = ranges,
    plotting_params = params$plotting,
    file_row = file_row,
    interval_name = interval_name,
    ch_id = ch_id
    '
  args <- list(...)
  cell_coords <- args$cell_coords
  period_tbl <- args$period_tbl
  auc_results <- args$auc_results
  file_row <- args$file_row
  ranges <- plotting_params$ranges
  interval_name <- args$interval_name
  ch_id <- args$ch_id
  colorcode <- args$color
  network_results <- args$network_results
  summary_results <- args$summary_results

  period_tbl2 <- args$period_tbl2
  ch_id2 <- args$ch_id2


  # TODO add a folder path to be returned for different categories of plots
  switch (plot_type,
          "period" = {
            # run checks on input
            colorcode <- ifelse(is.null(colorcode), "purple", colorcode)

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "period",
              filename = file_row$file_id,
              colorcode = colorcode,
              shape = 15, size = 2,
              lower_bound = ranges[[ch_id]]$period_range[1],
              upper_bound = ranges[[ch_id]]$period_range[2]
            )

            plot_name = "spatial_period"
            status <- ifelse(!is.null(plot), 1, 0)

          },
          "amplitude" = {
            # run checks on input

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "amplitude",
              filename = file_row$file_id,
              colorcode = "acqua",
              sdFactor = 3.5,
              shape = 15,
              size = 2,
              lower_bound = ranges[[ch_id]]$amplitude_range[1],
              upper_bound = ranges[[ch_id]]$amplitude_range[2]
            )

            plot_name = "spatial_amplitude"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "error" = {
            # run checks on input

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "error",
              filename = file_row$file_id,
              colorcode = "blue",
              sdFactor = 1.5,
              shape = 15,
              size = 2,
              lower_bound = ranges[[ch_id]]$error_range[1],
              upper_bound = ranges[[ch_id]]$error_range[2]
            )

            plot_name = "spatial_error"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "RAE" = {
            # run checks on input

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "RAE",
              filename = file_row$file_id,
              colorcode = "darkslategrey",
              shape = 15,
              size = 2,
              lower_bound = ranges[[ch_id]]$RAE_range[1],
              upper_bound = ranges[[ch_id]]$RAE_range[2]
            )

            plot_name = "spatial_RAE"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "AUC" = {
            # run checks on input
            # TODO add check that AUC has been calculated

            plot <- highlight_cells_period(
              period_table = auc_results,
              all_cells = cell_coords,
              variable = "AUC",
              filename = file_row$file_id,
              colorcode = "hotpink",
              shape = 15,
              size = 2,
              sdFactor = 2,
              lower_bound = ranges[[ch_id]]$AUC_range[1],
              upper_bound = ranges[[ch_id]]$AUC_range[2]
            )

            plot_name = "spatial_AUC"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase" = {
            # run checks on input

            plot <- highlight_cells_period_circ(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "phase_h",
              filename = file_row$file_id,
              shape = 15,
              size = 2,
              invert_color = TRUE
            )

            plot_name = "spatial_phases"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase_norm" = {
            # run checks on input

            plot <- highlight_cells_period_circ(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "phase_norm",
              filename = file_row$file_id,
              shape = 15,
              size = 2,
              normalized = TRUE,
              invert_color = TRUE
            )

            plot_name = "spatial_phases_norm"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase_circ" = {
            # run checks on input

            plot <- highlight_cells_period_circ(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "phase_circ",
              filename = file_row$file_id,
              shape = 15,
              size = 2,
              invert_color = TRUE
            )

            plot_name = "spatial_phases_circ"
            status <- ifelse(!is.null(plot), 1, 0)

          },
          "period_crn" = {
            # run checks on input
            # TODO add check that crn has been calculated

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "period_crn",
              filename = file_row$file_id,
              colorcode = "amethyst",
              shape = 15,
              size = 2,
              lower_bound = 0,
              upper_bound = 1
            )

            plot_name = "spatial_period_crn"
            status <- ifelse(!is.null(plot), 1, 0)


          },
          "amplitude_crn" = {
            # run checks on input
            # TODO add check that crn has been calculated

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "amp_crn",
              filename = file_row$file_id,
              colorcode = "darkseagreen",
              shape = 15,
              size = 2,
              lower_bound = 0,
              upper_bound = 1
            )

            plot_name = "spatial_amplitude_crn"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase_crn" = {
            # run checks on input
            # TODO add check that crn has been calculated

            plot <- highlight_cells_period(
              period_table = period_tbl,
              all_cells = cell_coords,
              variable = "phase_crn",
              filename = file_row$file_id,
              colorcode = "darkslategrey",
              shape = 15,
              size = 2,
              lower_bound = 0,
              upper_bound = 1
            )

            plot_name = "spatial_phase_crn"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "3D" = {
            # run checks on input

            plot <- highlight_cells_threeD(
              period_table = period_tbl,
              all_cells = cell_coords,
              var_z = "phase_norm",
              var_col = "period",
              filename = file_row$file_id,
              colorcode = "green",
              sdFactor = 1.5,
              size = 5,
              lower_bound = ranges[[ch_id]]$period_range[1],
              upper_bound = ranges[[ch_id]]$period_range[2]
            )

            plot_name = "3D_period_phase"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase_circular" = {
            # run checks on input

            align <- ifelse(is.null(plotting_params$align), FALSE, TRUE)
            align_to <- ifelse(is.null(plotting_params$align), FALSE, plotting_params$align[[ch_id]][1])
            # TODO add check on alignment parameter and output an error if wrong params
            # default to no alignment if wrong params

            if(align && align_to %in% names(params$channels) &&
               params$channels[[align_to]]$enabled) {
              reference_phase <- summary_results$phase_rad_mean[
                summary_results$channel_id == align_to &
                  summary_results$interval == interval_name]

              mean_phase <- summary_results$phase_rad_mean[
                summary_results$channel_id == ch_id &
                  summary_results$interval == interval_name]

              # calculate difference between
              phase_diff <- reference_phase - mean_phase

              # access the set phase for the reference reporter and align
              set_phase <- as.numeric(plotting_params$align[[align_to]][1])*(pi/12)
              final_phase <- set_phase - phase_diff
              align_to <- final_phase * (12/pi)
            }

            if(!is.null(align_to)) align_to <- as.numeric(align_to)

            if (length(params$time$intervals) > 1) {
              path <- file.path(
                file_row$folder_path, "plots", interval_name, ch_id,
                paste0(file_row$file_id, "_",
                       ch_id, "_rayleigh.svg")
              )
            } else {
              path <- file.path(
                file_row$folder_path, "plots", ch_id,
                paste0(file_row$file_id, "_",
                       ch_id, "_rayleigh.svg")
              )
            }

            circular_plot(data = period_tbl$phase_rad,
                          path = path,
                          saving = TRUE,
                          pt_col = params$plotting$colors[[ch_id]],
                          plot_title = file_row$file_id,
                          align = align,
                          align_to = align_to
                          )

            plot_name = "rayleigh"
            status <- NA
          },
          "phase_distribution" = {
            # run checks on input
            plot <- phase_distribution(data = period_tbl,
                                       color = params$plotting$colors[[ch_id]],
                                       Ch_rep = params$channels[[ch_id]]$label,
                                       filename = file_row$file_id,
                                       ylims = ranges[[ch_id]]$phase_distr_range
                                       )

            plot_name = "phase_distr"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "period_hist" = {
            # run checks on input
            plot <- hist_plot(var = period_tbl$period,
                              bin_size = 72,
                              bin_color = params$plotting$colors[[ch_id]],
                              x_label = "Period (h)",
                              y_label = "Count",
                              title = paste0(file_row$file_id, " - ",
                                             params$channels[[ch_id]]$label),
                              xlims = c(18, 36, 2),
                              mean = TRUE
                              )

            plot_name = "period_histogram"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          # Network plots
          "degree_hist" = {
            # run checks on input
            node_metrics_tbl <- network_results$network_vars$node_metrics

            plot <- hist_plot(
              var = node_metrics_tbl$degree,
              bin_color = "steelblue",
              x_label = "Degree",
              title = "Degree Distribution")

            plot_name = "degree_hist"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "strength_hist" = {
            # run checks on input
            node_metrics_tbl <- network_results$network_vars$node_metrics

            plot <- strength_hist <- hist_plot(
              var = node_metrics_tbl$strength,
              bin_color = "darkorange",
              x_label = "Strength",
              title = "Strenght Distribution")

            plot_name = "strength_hist"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "degree_weight_hist" = {
            # run checks on input
            node_metrics_tbl <- network_results$network_vars$node_metrics

            plot <- degree_weight_hist <- hist_plot(
              var = node_metrics_tbl$mean_edge_weight,
              bin_color = "darkseagreen",
              x_label = "Degree weighted",
              title = "Degree weighted Distribution")

            plot_name = "degree_weight_hist"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "spatial_node_weights" = {

            # run checks on input
            plot <- highlight_cells_period(
              all_cells = cell_coords,
              period_table = network_results$node_data,
              variable = "mean_edge_weight",
              filename = file_row$file_id,
              colorcode = "amethyst",
              shape = 15,
              size = 2,
              ID_col = "name")

            plot_name = "spatial_node_weights"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "spatial_node_strengths" = {
            # run checks on input
            plot <- highlight_cells_period(
              all_cells = cell_coords,
              period_table = network_results$node_data,
              variable = "strength",
              filename = file_row$file_id,
              colorcode = "acqua",
              shape = 15,
              size = 2,
              ID_col = "name")

            plot_name = "spatial_node_strengths"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "spatial_node_clust_coeff" = {
            # run checks on input
            plot <- highlight_cells_period(
              all_cells = cell_coords,
              period_table = network_results$node_data,
              variable = "clust_coef",
              filename = file_row$file_id,
              colorcode = "french_acqua",
              shape = 15,
              size = 2,
              ID_col = "name")

            plot_name = "spatial_node_clust_coeff"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "spatial_clusters" = {
            # run checks on input
            colors_map <- network_results$network_vars$cluster_df$color

            plot <- spatial_clusters_plot(network_results$node_data,
                                          colors_map,
                                          filename = file_row$file_id)

            plot_name = "spatial_clusters"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "clusters_phases" = {
            # run checks on input
            colors_map <- network_results$network_vars$cluster_df$color

            plot <- cluster_plot(data = network_results$node_data,
                                 x_var = "cluster",
                                 y_var = "phase_norm",
                                 color_var = "cluster",
                                 colorscale = colors_map,
                                 x_label = "Cluster",
                                 y_label = "Normalised phase (h)")

            plot_name = "clusters_phases"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "clusters_period" = {
            # run checks on input
            colors_map <- network_results$network_vars$cluster_df$color

            plot <- cluster_plot(data = network_results$node_data,
                                 x_var = "cluster",
                                 y_var = "period",
                                 color_var = "cluster",
                                 colorscale = colors_map,
                                 x_label = "Cluster",
                                 y_label = "Period (h)")

            plot_name = "clusters_period"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "clusters_amplitude" = {
            # run checks on input
            colors_map <- network_results$network_vars$cluster_df$color

            plot <- cluster_plot(data = network_results$node_data,
                                 x_var = "cluster",
                                 y_var = "amplitude",
                                 color_var = "cluster",
                                 colorscale = colors_map,
                                 x_label = "Cluster",
                                 y_label = "Amplitude (A.U.)")

            plot_name = "clusters_amplitude"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "clusters_per_crn" = {
            # run checks on input
            colors_map <- network_results$network_vars$cluster_df$color

            plot <- cluster_plot(data = network_results$node_data,
                                 x_var = "cluster",
                                 y_var = "period_crn",
                                 color_var = "cluster",
                                 colorscale = colors_map,
                                 x_label = "Cluster",
                                 y_label = "Period coherence (A.U.)")

            plot_name = "clusters_per_crn"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "cluster_hist" = { # TODO adjust warning
            # run checks on input

            plot <- hist_plot(
              var = network_results$node_data$cluster,
              bin_color = "#6EBD71",
              x_label = "Cluster",
              title = "Cluster Distribution",
              mode = "bar")

            plot_name = "cluster_hist"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "clusters_doughnut" = { # TODO check function
            # run checks on input

            colors_map <- network_results$network_vars$cluster_df[, c("cluster", "color")]

            plot <- doughnut_plot(node_data = network_results$node_data,
                                  colors_map = colors_map)

            plot_name = "clusters_doughnut"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "corr_plot" = {
            # run checks on input

            summary_nodes <- network_results$network_vars$summary_nodes
            numeric_data <- summary_nodes %>%
              select(where(is.numeric)) %>% .[-length(.),]

            cor_matrix <- cor(numeric_data,
                              use = "pairwise.complete.obs",
                              method = "pearson")
            suppressWarnings(
              plot <- ggcorrplot::ggcorrplot(
                cor_matrix,
                method = "circle",     # or "square"
                type = "lower",        # show lower triangle
                lab = TRUE,            # add correlation values
                lab_size = 4,
                colors = c("blue", "white", "red"),  # color scale
                title = "Correlation Matrix",
                ggtheme = theme_minimal())
            )

            plot_name = "corr_plot"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "overlap_hetmap" = {
            # run checks on input
            if(FALSE){

            segregation_ind <- network_results$network_vars$segregation_idx
            plot <- plot_overlap_heatmap(segregation_ind$overlap_matrix)
            }else{
              plot <- NULL
            }

            plot_name = "overlap_hetmap"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "two_phases_distribution" = {
            # run checks on input
            Ch1_color <- params$plotting$colors[ch_id]
            Ch2_color <- params$plotting$colors[ch_id2]
            Ch1_rep <- params$channels[ch_id]$label
            Ch2_rep <- params$channels[ch_id2]$label
            data1 <- period_tbl
            data2 <- period_tbl2
            ylim1 <- ranges[[ch_id]]$phase_distr_range
            ylim2 <- ranges[[ch_id2]]$phase_distr_range
            ylims <- c(min(ylim1[1], ylim2[1]),
                       max(ylim1[2], ylim2[2])
                       )

            # TODO plan what to do if more than 2 channels are being analysed
            plot <- phase_distribution(data = data1,
                                       color = Ch1_color,
                                       Ch_rep = Ch1_rep,
                                       multi = TRUE,
                                       data2 = data2,
                                       color2 = Ch2_color,
                                       Ch2_rep = Ch2_rep,
                                       filename = file_row$file_id,
                                       ylims = ylims)

            plot_name = "two_phases_distr"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "two_phases_rayleigh" = {
            # run checks on input

            # TODO plan what to do if more than 2 channels are being analysed
            # circular_plot(data = period_tbl$phase_rad,
            #               path = file.path(
            #                 file_row$folder_path, "plots", ch_id,
            #                 paste0(file_row$file_id, "_",
            #                        ch_id, "_rayleigh.svg")
            #               ),
            #               saving = TRUE,
            #               pt_col = params$plotting$colors[[ch_id]],
            #               plot_title = file_row$file_id
            # )

            plot_name = "two_phases_rayleigh"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "period_variance" = {
            # run checks on input

            plot_name = "period_var"
            status <- ifelse(!is.null(plot), 1, 0)
          },
          "phase_variance" = {
            # run checks on input

            plot_name = "phase_var"
            status <- ifelse(!is.null(plot), 1, 0)
          }
  )

  # return list
  list(plot = plot,
       plot_name = plot_name,
       status = status)

}

#' Spatial map of a period-analysis variable with linear color scale
#'
#' @param all_cells Data frame (or matrix) with three columns: cell ID, X
#'   coordinate, and Y coordinate.
#' @param period_table Data frame of period-analysis results; must contain a
#'   column matching \code{ID_col}.
#' @param variable Character; name of the column in \code{period_table} to
#'   map to color.
#' @param filename Character; used in the plot title. Defaults to \code{""}.
#' @param colorcode Character; color used for the gradient high end. Defaults
#'   to \code{"blue"}.
#' @param sdFactor Numeric; number of SDs around the mean used to clip the
#'   color scale. Defaults to \code{1}.
#' @param shape Integer; point shape code. Defaults to \code{15}.
#' @param size Numeric; point size. Defaults to \code{2}.
#' @param ID_col Character; name of the ID column shared between
#'   \code{all_cells} and \code{period_table}. Defaults to \code{"ID"}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
highlight_cells_period <- function(all_cells, period_table, variable, filename = "", colorcode = "blue", sdFactor = 1, shape=15, size = 2, ID_col = "ID", ...){

  plot_data <- all_cells %>% `colnames<-`(c(ID_col, "X", "Y"))
  # Merge tables to include period data
  XY_cols = all(c("X", "Y") %in% names(period_table))
  if(XY_cols){
    #' remove XY cols to allow smooth merging
    period_table <- period_table %>% select(-c("X", "Y"))
  }

  merged_data <- left_join(plot_data, period_table, by = ID_col)

  # Choose the variable to visualize (e.g., period, amplitude, error)
  variable <- variable

  mean_value <- mean(merged_data[[variable]], na.rm = TRUE)
  sd_value <- sd(merged_data[[variable]], na.rm = TRUE)

  args = list(...)
  upper_bound <- args$upper_bound
  lower_bound <- args$lower_bound

  if(is.null(upper_bound)){
    upper_bound <- mean_value + (sdFactor * sd_value)
  }
  if(is.null(lower_bound)){
    lower_bound <- mean_value - (sdFactor * sd_value)
    lower_bound <- ifelse(lower_bound < 0, 0, lower_bound)
  }
  colorschemes <- list("blue" = c("#DEEBF5", "#A0C9DF", "#559ECA", "#1663A5", "#003066"),
                       "green" = c("#F1F9EE", "#C0E5BB", "#6EBD71", "#07421F", "#07421F"),
                       "orange" = c("#FFEFDB", "#FCB37E", "#F5734C", "#D12B22", "#7A0006"),
                       "purple" = c("#FDEAE6", "#FAA5B0", "#F45F96", "#B21978", "#48065F"),
                       "red" = c("#FFEDE5", "#FAB29A", "#FD5C47", "#C81B21", "#640112"),
                       "violet" = c("#FAF9FC", "#B7BAD8", "#8E8CBD", "#685DA2", "#371071"),
                       "amethyst" = c("#F0D6FF", "#DCADFF", "#B376E5", "#7C33BC", "#5A189A", "#3C096C"),
                       "french_acqua" = c("#80FFDB", "#72EFDD", "#64DFDF", "#56CFE1", "#48BFE3", "#4EA8DE", "#5390D9", "#5E60CE", "#6930C3", "#7400B8"),
                       "acqua" = c("#F0F9E9", "#B9E2BA", "#6EC1BD", "#217EB2", "#003B74"),
                       "khaki" = c("#FDFAD3", "#EFE79E", "#EEE685", "#CDC673", "#8B864E"),
                       "darkseagreen" = c("#D6FFD6", "#BCF0BC", "#92C892", "#73AB73", "#587458"),
                       "cornsilk" = c("ivory", "cornsilk", "cornsilk2", "cornsilk3", "cornsilk4"),
                       "darkslategrey" = c("#97FFFF","#8DEEEE","#77C9C9","#528B8B","#2F4F4F"),
                       "hotpink" = c("#FFEBF5", "#FFC2E0", "#FD9BCA", "hotpink3", "hotpink4"),
                       "royalblue" = c("#A7B9F1", "#7094FF", "royalblue2", "royalblue3", "royalblue4"),
                       "chartreuse" = c("#B8FF70", "#8FFF1F", "chartreuse2", "chartreuse3", "chartreuse4")
  )
  colorscheme = colorschemes[[colorcode]]
  # Add a new variable to handle outliers and scaling
  merged_data <- merged_data %>%
    mutate(
      alphaval = ifelse(is.na(!!sym(variable)), 0, 1)
    )

  plot <- ggplot(merged_data, aes(x = X, y = Y, alpha = alphaval)) +
    geom_point(shape = shape, aes(colour = !!sym(variable), size = size)) +
    scale_color_gradientn(
      colors = colorscheme,
      values = scales::rescale(c(lower_bound, upper_bound)),
      limits = c(lower_bound, upper_bound),
      oob = scales::squish  # Ensures outliers are squished into the limits
    ) +
    scale_alpha_identity()+
    scale_size_identity()+
    theme_minimal() +
    labs(
      title = paste0(filename, " - Distribution of ", variable),
      x = "Width (\u00b5m)",
      y = "Height (\u00b5m)",
    ) +
    coord_fixed() +
    scale_y_reverse()+
    theme(plot.title = element_text(size = 11),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 15))

  return(plot)
}

#' Spatial map of a period-analysis variable with circular (phase) color scale
#'
#' @param all_cells Data frame (or matrix) with three columns: cell ID, X
#'   coordinate, and Y coordinate.
#' @param period_table Data frame of period-analysis results; must contain a
#'   column named \code{"ID"}.
#' @param variable Character; name of the column in \code{period_table} to
#'   map to color.
#' @param filename Character; used in the plot title. Defaults to \code{""}.
#' @param shape Integer; point shape code. Defaults to \code{15}.
#' @param size Numeric; point size. Defaults to \code{2}.
#' @param normalized Logical; if \code{TRUE}, the variable is scaled to
#'   \code{[0, 1]} before mapping. Defaults to \code{FALSE}.
#' @param colorcode Character; name of the color scheme to use. One of
#'   \code{"rainbow"}, \code{"regular"}, \code{"inverse"}, or
#'   \code{"grayscale"}. Defaults to \code{"rainbow"}.
#' @param invert_color Logical; if \code{TRUE}, reverses the color palette.
#'   Defaults to \code{FALSE}.
#' @param ID_col Character; name of the ID column. Defaults to \code{"ID"}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
highlight_cells_period_circ <- function(all_cells, period_table, variable, filename = "", shape=15, size = 2, normalized = FALSE, colorcode = "rainbow", invert_color = FALSE, ID_col = "ID", ...){

  plot_data <- all_cells %>% `colnames<-`(c(ID_col, "X", "Y"))
  # Merge tables to include period data
  merged_data <- left_join(plot_data, period_table, by = "ID")

  colorschemes <- list("rainbow" = c("#ff0000", "#ff7e00", "#fffc00", "#7eff00", "#00ff00", "#00ff7e", "#00fcfe", "#007eff", "#0000ff", "#8400ff", "#ff00fc", "#ff007e", "#ff0000"),
                       "regular" = c("#04305C", "#245380", "#4376A4", "#8BB09D", "#D2EA96", "#E8F4A8", "#FEFEB9", "#FEDD96", "#FDBC73", "#E08258", "#C2473D", "#8E3334", "#591F2B"),
                       "inverse" = c("#8BB09D", "#D2EA96", "#E8F4A8", "#FEFEB9", "#FEDD96", "#FDBC73", "#E08258", "#C2473D", "#8E3334", "#591F2B", "#2C2640", "#04305C", "#245380", "#4376A4"),
                       "grayscale" = c("#010101", "#FFFFFF")
  )

  colorscheme = colorschemes[[colorcode]]

  if(normalized == "grayscale"){
    colorscheme = colorschemes[["grayscale"]]
  }
  merged_data <- merged_data %>%
    mutate(
      alphaval = ifelse(is.na(!!sym(variable)), 0, 1)
    )
  # Choose the variable to visualize (e.g., period, amplitude, error)
  variable <- variable

  # Create the plot
  plot <- ggplot(merged_data, aes(x = X, y = Y, alpha = alphaval)) +
    geom_point(shape = shape, aes(color = !!sym(variable)), size = size) +
    theme_minimal() +
    scale_alpha_identity()+
    scale_size_identity()+
    labs(title = paste0(filename, " - Distribution of ", variable), x = "Width (\u00b5m)", y = "Height (\u00b5m)") +
    coord_fixed() +
    scale_y_reverse()+
    theme(plot.title = element_text(size = 11),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 15))

  if(normalized == TRUE){
    plot <- plot +
      scale_color_gradientn(colors = colorscheme, limits = c(-12.15, 12.15), breaks = c(-12, -6, 0, 6, 12))
  } else if(normalized == FALSE){
    plot <- plot +
      scale_color_gradientn(colors = colorscheme, limits = c(-0.15, 24.15), breaks = c(0, 6, 12, 18, 24))
  } else if(normalized == "grayscale"){
    plot <- plot +
      scale_color_gradientn(colors = colorscheme, limits = c(-12.15, 12.15), breaks = c(-12, -6, 0, 6, 12))+
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())+
      labs(title = paste0(filename, " - Normalized phases grayscale"))
  }

  return(plot)
}

#' 3D scatter map with XY spatial position and a Z-axis variable
#'
#' @param all_cells Data frame of grid coordinates (defaults to
#'   \code{grid_coord}); three columns: ID, X, Y.
#' @param period_table Data frame of period-analysis results containing
#'   \code{var_z} and \code{var_col} columns.
#' @param var_z Character; column name mapped to the Z axis.
#' @param var_col Character; column name mapped to point color.
#' @param filename Character; used in the plot title. Defaults to \code{""}.
#' @param colorcode Character; color palette name for the color axis. Defaults
#'   to \code{"blue"}.
#' @param sdFactor Numeric; number of SDs used to clip the Z-axis range.
#'   Defaults to \code{1}.
#' @param shape Integer; point shape code. Defaults to \code{15}.
#' @param size Numeric; point size. Defaults to \code{2}.
#' @param outlier_mode Character; how out-of-range values are handled;
#'   \code{"squish"} (default) or \code{"na"}.
#' @param xlim Numeric vector of length 2; X-axis limits for the color scale.
#'   Defaults to \code{c(-12, 12)}.
#' @param ID_col Character; name of the ID column. Defaults to \code{"ID"}.
#' @param ... Currently unused.
#'
#' @return A \code{plotly} figure object.
#' @keywords internal
highlight_cells_threeD <- function(all_cells = grid_coord, period_table, var_z, var_col, filename = "", colorcode = "blue", sdFactor = 1, shape = 15, size = 2, outlier_mode = "squish", xlim = c(-12, 12),  ID_col = "ID", ...){

  plot_data <- grid_coord %>% `colnames<-`(c(ID_col, "X", "Y"))

  XY_cols = all(c("X", "Y") %in% names(period_table))
  if(XY_cols){
    #' remove XY cols to allow smooth merging
    period_table <- period_table %>% select(-c("X", "Y"))
  }

  # Merge tables to include period data
  merged_data <- left_join(plot_data, period_table, by = ID_col)

  # Choose the variable to visualize (e.g., period, amplitude, error)
  variable_z <- var_z
  variable_col <- var_col

  plot_var_z <- formula(paste0("~", variable_z))
  plot_var_col <- formula(paste0("~", variable_col))

  args = list(...)
  upper_bound <- args$upper_bound
  lower_bound <- args$lower_bound

  numeric = is.numeric(merged_data[[var_col]])
  if(numeric){
    mean_value <- mean(merged_data[[var_col]], na.rm = TRUE)
    sd_value <- sd(merged_data[[var_col]], na.rm = TRUE)

    if(is.null(upper_bound)){
      upper_bound <- mean_value + (sdFactor * sd_value)
    }
    if(is.null(lower_bound)){
      lower_bound <- mean_value - (sdFactor * sd_value)
      lower_bound <- ifelse(lower_bound < 0, 0, lower_bound)
    }
    # Handling outliers
    if (outlier_mode == "squish") {
      merged_data <- merged_data %>%
        mutate(color_var = pmin(pmax(!!sym(var_col), lower_bound), upper_bound))
    } else if (outlier_mode == "categorical") {
      merged_data <- merged_data %>%
        mutate(color_var = case_when(
          !!sym(var_col) < lower_bound ~ lower_bound,
          !!sym(var_col) > upper_bound ~ upper_bound,
          TRUE ~ !!sym(var_col)
        ))
    } else {
      stop("Invalid outlier_mode. Choose 'squish' or 'categorical'.")
    }
  }


  colorschemes <- list("blue" = c("#DEEBF5", "#A0C9DF", "#559ECA", "#1663A5", "#003066"),
                       "green" = c("#F1F9EE", "#C0E5BB", "#6EBD71", "#07421F", "#07421F"),
                       "orange" = c("#FFEFDB", "#FCB37E", "#F5734C", "#D12B22", "#7A0006"),
                       "purple" = c("#FDEAE6", "#FAA5B0", "#F45F96", "#B21978", "#48065F"),
                       "red" = c("#FFEDE5", "#FAB29A", "#FD5C47", "#C81B21", "#640112"),
                       "violet" = c("#FAF9FC", "#B7BAD8", "#8E8CBD", "#685DA2", "#371071"),
                       "acqua" = c("#F0F9E9", "#B9E2BA", "#6EC1BD", "#217EB2", "#003B74"),
                       "khaki" = c("#FEF8AE", "#EFE79E", "#EEE685", "#CDC673", "#8B864E"),
                       "darkseagreen" = c("#D6FFD6", "#BCF0BC", "#92C892", "#73AB73", "#587458"),
                       "cornsilk" = c("ivory", "cornsilk", "cornsilk2", "cornsilk3", "cornsilk4"),
                       "darkslategrey" = c("#97FFFF","#8DEEEE","#77C9C9","#528B8B","#2F4F4F"),
                       "hotpink" = c("#FFC2E0", "#FD9BCA", "hotpink2", "hotpink3", "hotpink4"),
                       "royalblue" = c("#A7B9F1", "#7094FF", "royalblue2", "royalblue3", "royalblue4"),
                       "chartreuse" = c("#B8FF70", "#8FFF1F", "chartreuse2", "chartreuse3", "chartreuse4"))
  if(colorcode == "other"){
    colorscheme = args$custom_col
  }else{
    colorscheme = colorschemes[[colorcode]]
  }

  # handle x and y axis limits to adjust plot aspect
  x_max = max(merged_data$X)
  y_max = max(merged_data$Y)
  y_min = min(merged_data$Y)
  y_ratio = y_max/x_max

  marker_list <- list(size = size, symbol = shape)

  if(numeric){
    marker_list$colorbar <- list(title = "Colorbar")
    marker_list$cmin <- lower_bound
    marker_list$cmax <- upper_bound
  }

  # Create the plot
  plot <- plot_ly(merged_data,
                  x = plot_var_z,
                  y = ~X,
                  z = ~Y,
                  color = plot_var_col,
                  colors = colorscheme,
                  type = "scatter3d",
                  mode = "markers",
                  marker = marker_list) %>%
    layout(scene =
             list(xaxis = list(title = "Phase (h)",  range = xlim, tickvals = seq(from = min(xlim), to = max(xlim), by = 4)),
                  yaxis = list(title = "Width (\u00b5m)"),
                  zaxis = list(title = "Height (\u00b5m)",
                               range = c(y_max, y_min)),
                  title = paste0(filename, " - Distribution of ", variable_z),
                  aspectmode = "manual",
                  aspectratio = list(x = 1, y = 1, z = y_ratio))
    )

  return(plot)


}

#' 3D scatter map combining two channels in a single plotly figure
#'
#' @param all_cells Data frame of grid coordinates (defaults to
#'   \code{grid_coord}); three columns: ID, X, Y.
#' @param period_table1 Data frame of period-analysis results for channel 1.
#' @param period_table2 Data frame of period-analysis results for channel 2.
#' @param var_z Character; column name mapped to the Z axis.
#' @param var_col Character; column name mapped to point color.
#' @param filename Character; used in the plot title. Defaults to \code{""}.
#' @param sdFactor Numeric; number of SDs used to clip the Z-axis range.
#'   Defaults to \code{1.5}.
#' @param shape Integer; point shape code. Defaults to \code{15}.
#' @param size Numeric; point size. Defaults to \code{10}.
#' @param Ch1_name Character; label for channel 1 in the legend. Defaults to
#'   \code{"Ch1"}.
#' @param Ch2_name Character; label for channel 2 in the legend. Defaults to
#'   \code{"Ch2"}.
#' @param xlim Numeric vector of length 2; limits for the color axis. Defaults
#'   to \code{c(-12, 12)}.
#' @param ... Currently unused.
#'
#' @return A \code{plotly} figure object.
#' @keywords internal
highlight_cells_threeD_merge <- function(all_cells = grid_coord, period_table1, period_table2, var_z, var_col, filename = "", sdFactor = 1.5, shape = 15, size = 10, Ch1_name = "Ch1", Ch2_name = "Ch2", xlim = c(-12, 12),...){

  plot_data <- grid_coord %>% `colnames<-`(c("ID", "X", "Y"))
  # Merge tables to include period data
  merged_data_ch1 <- left_join(plot_data, period_table1, by = "ID")
  merged_data_ch2 <- left_join(plot_data, period_table2, by = "ID")

  # merge the two tables adding a channel variable
  merged_data_ch1$channel <- "Ch1"
  merged_data_ch2$channel <- "Ch2"
  merged_data <- rbind(merged_data_ch1, merged_data_ch2)

  # Choose the variable to visualize (e.g., period, amplitude, error)
  variable_z <- var_z
  variable_col <- var_col

  plot_var_z <- formula(paste0("~", variable_z))
  plot_var_col <- formula(paste0("~", variable_col))

  # get coloscheme

  plot_title = paste0("3D plot of ", Ch1_name, " and ", Ch2_name, ", ", var_z, " by ", var_col)

  # Prepare the dataset (keeping it in long format for clarity)
  dataset_long <- merged_data %>%
    select(ID, X, Y, channel, !!variable_z, !!variable_col) %>%
    rename(Z = !!variable_z, color = !!variable_col)

  # dataset_long1 <- merged_data_ch1 %>%
  #   select(ID, X, Y, channel, !!variable_z, !!variable_col) %>%
  #   rename(Z = !!variable_z, color = !!variable_col)
  #
  # dataset_long2 <- merged_data_ch2 %>%
  #   select(ID, X, Y, channel, !!variable_z, !!variable_col) %>%
  #   rename(Z = !!variable_z, color = !!variable_col)
  # dataset_long2$ID <- dataset_long2$ID + 8000

  # Compute mean and SD for color (Ch1 and Ch2)
  color_Ch1_mean <- mean(dataset_long$color[dataset_long$channel == "Ch1"], na.rm = TRUE)
  color_Ch1_sd <- sd(dataset_long$color[dataset_long$channel == "Ch1"], na.rm = TRUE)

  color_Ch2_mean <- mean(dataset_long$color[dataset_long$channel == "Ch2"], na.rm = TRUE)
  color_Ch2_sd <- sd(dataset_long$color[dataset_long$channel == "Ch2"], na.rm = TRUE)

  # Define the color range (mean \u00b1 k*SD)
  color_Ch1_range <- c(color_Ch1_mean - sdFactor * color_Ch1_sd, color_Ch1_mean + sdFactor * color_Ch1_sd)
  color_Ch2_range <- c(color_Ch2_mean - sdFactor * color_Ch2_sd, color_Ch2_mean + sdFactor * color_Ch2_sd)

  # handle x and y axis limits to adjust plot aspect
  x_max = max(merged_data$X)
  y_max = max(merged_data$Y)
  y_min = min(merged_data$Y)
  y_ratio = y_max/x_max

  # Create the plot
  plot <- plot_ly(
    text = ~paste0(variable_z,": ", Z,
                   "<br>", variable_col,": ", color,
                   "<br>ID: ", ID),
    hoverinfo = 'text') %>% #TODO add the corresponding cell in the other channel to lit up
    add_trace(
      data = dataset_long %>% filter(channel == "Ch1"),
      x = ~Z,
      y = ~X,
      z = ~Y,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = size,
        symbol = shape,
        color = ~color,
        colorscale = "Viridis",  # Simple colorscale for Ch1
        colorbar = list(
          title = Ch1_name,
          yanchor = "bottom",
          len = 0.5
        ),
        cmin = color_Ch1_range[1],  # Set color scale minimum
        cmax = color_Ch1_range[2]   # Set color scale maximum
      )#,
      #name = FALSE
    ) %>%
    add_trace(
      data = dataset_long %>% filter(channel == "Ch2"),
      x = ~Z,
      y = ~X,
      z = ~Y,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = size,
        symbol = shape,
        color = ~color,
        colorscale = "Inferno",  # Simple colorscale for Ch2
        colorbar = list(
          title = Ch2_name,
          yanchor = "bottom",
          len = 0.5,
          x = 1.2  # Adjust x position for the second colorbar
        ),
        cmin = color_Ch2_range[1],  # Set color scale minimum
        cmax = color_Ch2_range[2]   # Set color scale maximum
      )#,
      #name = FALSE
    ) %>%
    layout(
      scene = list(
        xaxis = list(title = "Phase (h)", titlefont = list(size = 14), range = xlim, tickvals = seq(from = min(xlim), to = max(xlim), by = 4)),
        yaxis = list(title = "Width (\u00b5m)", titlefont = list(size = 14)),
        zaxis = list(title = "Height (\u00b5m)", titlefont = list(size = 14), range = c(y_max, y_min)),
        camera = list(eye = eye),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = y_ratio)
      ),
      title = list(
        text = plot_title,
        font = list(size = 20),
        y = 0.95  # Lower the title
      ),
      showlegend = FALSE
    )


  return(plot)
}


#' Boxplot of a metric stratified by group with optional significance brackets
#'
#' @param data Data frame containing \code{metric} and \code{grouping_var}.
#' @param metric Unquoted column name of the numeric variable to plot.
#' @param grouping_var Unquoted column name of the grouping factor.
#' @param ylabel Character; y-axis label. Defaults to \code{""}.
#' @param xlabel Character; x-axis label. Defaults to \code{""}.
#' @param comparison Logical; whether to add pairwise significance brackets.
#'   Defaults to \code{TRUE}.
#' @param test Character; p-value correction method passed to the stat test
#'   (e.g. \code{"bonferroni"}). Defaults to \code{"bonferroni"}.
#' @param norm_test Logical; whether to run a Shapiro–Wilk normality test
#'   before selecting the statistical test. Defaults to \code{TRUE}.
#' @param plot_colors Named character vector of colors, one per group level.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
metric_boxplot <- function(data, metric, grouping_var, ylabel = "", xlabel = "", comparison = TRUE, test = "bonferroni", norm_test = TRUE, plot_colors, ...){

  # in referring to variables within the dplyr context, use the curly-curly syntax
  # when using them as part of strings, use the deparse(substitute()) syntax
  # when needed on add_xy_position, use as_label(enquo())

  if(norm_test){ # test for normality
    normality = data %>%  group_by({{grouping_var}}) %>% rstatix::shapiro_test({{metric}})
    #print(normality)
  }
  # compute anova
  formula = as.formula(paste(deparse(substitute(metric)), "~", deparse(substitute(grouping_var))))
  within_var_sym <- rlang::sym(deparse(substitute(grouping_var)))
  res.aov = rstatix::anova_test(data, formula = formula, wid = ID, within = !!within_var_sym)
  # perform pairwise comparison
  pwc = data %>% rstatix::pairwise_t_test(formula, paired = FALSE, p.adjust.method = test)
  # add xy position of pwc
  # xvar <- as_label(enquo(grouping_var))
  pwc <- pwc %>% rstatix::add_xy_position(x = as_label(enquo(grouping_var)), fun = "max", step.increase = 0.8)

  # generate plot
  plot <- ggpubr::ggboxplot(data, x = deparse(substitute(grouping_var)), y = deparse(substitute(metric)), color = deparse(substitute(grouping_var))) +
    ggpubr::stat_pvalue_manual(pwc)+
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    ggplot2::labs(
      subtitle = rstatix::get_test_label(res.aov, detailed = TRUE),
      caption = rstatix::get_pwc_label(pwc)
    )+
    ggplot2::scale_color_manual(values = plot_colors)
  ggplot2::theme(plot.subtitle = element_text(size = 10, hjust = 0))

  return(plot)
}

#' Mean ± SE plot of a metric stratified by group with optional significance brackets
#'
#' @param data Data frame containing \code{metric} and \code{grouping_var}.
#' @param metric Unquoted column name of the numeric variable to plot.
#' @param grouping_var Unquoted column name of the grouping factor.
#' @param ylabel Character; y-axis label. Defaults to \code{""}.
#' @param xlabel Character; x-axis label. Defaults to \code{""}.
#' @param comparison Logical; whether to add pairwise significance brackets.
#'   Defaults to \code{TRUE}.
#' @param test Character; p-value correction method (e.g.
#'   \code{"bonferroni"}). Defaults to \code{"bonferroni"}.
#' @param norm_test Logical; whether to run a Shapiro–Wilk normality test
#'   before selecting the statistical test. Defaults to \code{TRUE}.
#' @param plot_colors Named character vector of colors, one per group level.
#' @param user_ylims Numeric vector of length 2 to override automatic y-axis
#'   limits. Defaults to \code{NULL}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @export
metric_mean_plot <- function(data, metric, grouping_var, ylabel = "", xlabel = "", comparison = TRUE, test = "bonferroni", norm_test = TRUE, plot_colors, user_ylims = NULL, ...){

  # Compute mean and standard error for each group
  summary_stats <- data %>%
    dplyr::group_by({{grouping_var}}) %>%
    dplyr::summarise(
      mean = mean({{metric}}, na.rm = TRUE),
      se = sd({{metric}}, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  if (norm_test) { # Test for normality
    normality = data %>% group_by({{grouping_var}}) %>% rstatix::shapiro_test({{metric}})
    #print(normality)
  }

  # Compute ANOVA
  formula = as.formula(paste(deparse(substitute(metric)), "~", deparse(substitute(grouping_var))))
  within_var_sym <- rlang::sym(deparse(substitute(grouping_var)))
  res.aov = rstatix::anova_test(data, formula = formula, wid = ID, within = !!within_var_sym)

  # Perform pairwise comparison
  pwc = data %>% rstatix::pairwise_t_test(formula, paired = FALSE, p.adjust.method = test)

  # Add xy position of pwc
  pwc <- pwc %>% rstatix::add_xy_position(x = as_label(enquo(grouping_var)), fun = "mean_se", step.increase =0.18)

  # Generate plot with means and standard errors
  plot <- ggplot(summary_stats, aes(x = {{grouping_var}}, y = mean, color = {{grouping_var}})) +
    geom_point(size = 3, alpha = 0.7) +  # Add points for the means
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 1, ) +  # Add error bars
    ggplot2::scale_color_manual(values = plot_colors) +  # Apply custom colors
    ggplot2::labs(
      y = ylabel,
      x = xlabel,
      title = "Mean with Standard Error",
      subtitle = rstatix::get_test_label(res.aov, detailed = TRUE),
      caption = rstatix::get_pwc_label(pwc)
    ) +
    ggpubr::stat_pvalue_manual(pwc) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.subtitle = element_text(size = 10, hjust = 0)
    )

  if(!is.null(user_ylims)){
    plot = plot + ylim(user_ylims)
  }

  return(plot)
}

# density plot for phase distribution
#' Kernel density plot of phase distribution for one or two channels
#'
#' @param data Numeric vector of phases in radians for the primary channel.
#' @param color Character; line color for the primary channel. Defaults to
#'   \code{"#eb1020"}.
#' @param line_size Numeric; line width. Defaults to \code{1.2}.
#' @param bandwidth Numeric; kernel bandwidth passed to \code{geom_density()}.
#'   Defaults to \code{0.4}.
#' @param Ch_rep Character; label for the primary channel in the legend.
#'   Defaults to \code{"Ch1"}.
#' @param multi Logical; if \code{TRUE}, overlays a second density curve
#'   using \code{data2}. Defaults to \code{FALSE}.
#' @param data2 Numeric vector of phases in radians for the second channel;
#'   used when \code{multi = TRUE}. Defaults to \code{NA}.
#' @param color2 Character; line color for the second channel. Defaults to
#'   \code{"#159c10"}.
#' @param Ch2_rep Character; label for the second channel in the legend.
#'   Defaults to \code{"Ch2"}.
#' @param ylims Numeric vector of length 2 for y-axis limits, or \code{NA}
#'   for automatic limits. Defaults to \code{NA}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @export
phase_distribution <- function(data,
                              color = '#eb1020',
                              line_size = 1.2,
                              bandwidth = 0.4,
                              Ch_rep = "Ch1",
                              multi = FALSE,
                              data2 = NA,
                              color2 = '#159c10',
                              Ch2_rep = "Ch2",
                              ylims = NA,
                              ...
                              )
{

  args <- list(...)
  filename <- args$filename
  filename <- if (is.null(filename)) "sample" else filename

  ifelse(!(nrow(data) < 1),
         data$channel <- Ch_rep,
         data$channel <- factor())

  if (multi && !is.na(data2)[1]) {
    # TODO add check that both datasets are non-empty
    if(!(nrow(data) < 1) && !(nrow(data2) < 1)){
    data$channel <- Ch_rep
    data2$channel <- Ch2_rep
    combined_data <- rbind(data, data2)
    } else { combined_data <- data }
  } else {
    combined_data <- data
  }

  # Ensure channel is a factor with expected levels
  expected_levels <- if (multi) c(Ch_rep, Ch2_rep) else c(Ch_rep)
  combined_data$channel <- factor(combined_data$channel,
                                  levels = expected_levels)

  # TODO replace with von mises KDE circular::density.circular()

  plot <- ggplot(combined_data, aes(x = phase_norm, color = channel)) +
    geom_density(linewidth = line_size,
                 bw = bandwidth,
                 adjust = 1,
                 na.rm = TRUE) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Phase Distribution for", filename),
      x = "Phase distance from mean (h)",
      y = "Distribution of phases (KDE)",
      color = NULL
    ) +
    scale_x_continuous(
      limits = c(-12, 12),
      breaks = seq(-12, 12, by = 4)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(linewidth = 1, colour = "black"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      axis.ticks = element_line(linewidth = 0.8),
      axis.ticks.length = unit(5, "pt"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 14),
      legend.title = element_blank()
    )

  if (nrow(combined_data) > 0) {
    plot <- plot + scale_color_manual(
      values = setNames(c(color, color2)[seq_along(expected_levels)],
                        expected_levels)
    )
  }

  if(!is.na(ylims)[1]){
    plot <- plot + ylim(ylims)
  }

  return(plot)
}

# group traces plot
#' Mean ± SE trace plot for each group
#'
#' @param summary_data Data frame with columns \code{t} (time), \code{mean},
#'   \code{se}, and \code{group} (factor or character grouping variable).
#'
#' @return A \code{ggplot} object.
#' @export
plot_group_traces <- function(summary_data){
  plot = ggplot2::ggplot(summary_data)+
    geom_ribbon(aes(x = t,
                    ymin = mean-se,
                    ymax = mean+se,
                    alpha = 0.5,
                    colour = group,
                    fill = group)
                )+
    geom_line(aes(x = t, y = mean, colour = group))+
    ggplot2::scale_alpha_identity()+
    scale_x_continuous(name = "Time (h)",
                       breaks = seq(0, as.integer(max(summary_data$t)), by = 24),
                       minor_breaks = seq(12, as.integer(max(summary_data$t)), by = 24))+
    theme_minimal()+
    theme(axis.line.y = element_line(linewidth = 1),
          axis.line.x = element_line(linewidth = 1),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.5,
                                            linetype = "88",
                                            colour = "black"),
          plot.subtitle = element_text(size = 10, hjust = 0),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x  = element_text(size = 20),
          axis.title.y  = element_text(size = 20))

  return(plot)
}

# create heatmap plot from matrix
#' Cluster segregation heatmap
#'
#' @param overlap_matrix Square numeric matrix of pairwise cluster segregation
#'   scores; row and column names are cluster labels.
#'
#' @return A \code{ggplot} object.
#' @export
plot_overlap_heatmap <- function(overlap_matrix) {
  df <- as.data.frame(overlap_matrix)
  df$Cluster1 <- rownames(df)

  df_long <- df %>%
    pivot_longer(-Cluster1, names_to = "Cluster2", values_to = "Segregation")

  ggplot(df_long, aes(x = Cluster1, y = Cluster2, fill = Segregation)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
    theme_minimal() +
    labs(title = "Cluster Segregation Heatmap",
         x = "Cluster", y = "Cluster", fill = "Segregation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# print period lengths
#' Jitter plot of per-cell period lengths
#'
#' @param data Data frame with a \code{period} column (numeric, in hours).
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @export
period_print <- function(data, ...){
  df <- data
  filename
  cells = "Cells"

  min_y = round((min(data$period, na.rm = TRUE)-1), 0)
  max_y = round((max(data$period, na.rm = TRUE)+1), 0)
  if(is.infinite(min_y)){min_y = 20}
  if(is.infinite(max_y)){max_y = 28}
  plot1 <- ggplot2::ggplot(df, ggplot2::aes(cells, period)) +
    ggplot2::geom_jitter(alpha=.5)+
    labs(title = filename,
         y = "Period length (h)") +                      # Add axis labels and a title
    ggpubr::theme_pubclean()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), # Center and style the title
      axis.title.x = element_text(size = 18, face = "bold"),            # Style x-axis title
      axis.title.y = element_text(size = 18, face = "bold"),            # Style y-axis title
      axis.text.x = element_text(size = 0),                            # Style x-axis text
      axis.text.y = element_text(size = 16),
      axis.line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent")
    )+
    scale_y_continuous(limits = c(min_y, max_y),
                       expand = c(0, 0),
                       breaks = seq(from = min_y, to = max_y, by = 2))
  return(plot1)
}


#' Plot the fluorescence trace of a single cell
#'
#' @param cellID Integer or character; the cell identifier to look up in
#'   \code{data}.
#' @param data Data frame (long format with columns \code{ID}, \code{t},
#'   \code{value}) or numeric matrix (wide format, cells × frames).
#' @param title Character; plot title. Defaults to \code{""}.
#' @param tbl.type Character; \code{"long"} (default) or \code{"wide"}.
#' @param xlab Character; x-axis label. Defaults to \code{"Time (h)"}.
#' @param ylab Character; y-axis label. Defaults to \code{"Value"}.
#' @param ... Currently unused.
#'
#' @return A \code{ggplot} object.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   plot_cell(2245, period_res$traces, tbl.type = "wide")
#'   plot_cell(2245, channel_ctx$grid_vals, tbl.type = "wide")
#' }
#'
plot_cell <- function(cellID,
                     data,
                     title = "",
                     tbl.type = "long",
                     xlab = "Time (h)",
                     ylab = "Value", ...){

  # Filter data for the current ID
  if(tbl.type == "long"){
    data_ID <- data[data$ID == cellID, ]
    val = data_ID$value
    t = data_ID$t
    data_plot <- data.frame(t = t, value = val)
  } else if(tbl.type == "wide"){
    data_ID <- data[which(rownames(data) == cellID),]
    t = seq(0, length.out = length(data_ID), by = 0.5)
    data_plot <- data.frame(t = t, value = data_ID)
  }

  # Create the plot
  plot <- ggplot(data_plot, aes(x = t, y = value)) +
    geom_line() +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()

  return(plot)
}

# function to create 3D plots with plotly
#' Interactive 3D scatter plot via plotly
#'
#' @param data Data frame containing the variables to plot.
#' @param x_var Character; column name for the X axis.
#' @param y_var Character; column name for the Y axis.
#' @param z_var Character; column name for the Z axis.
#' @param color_var Character; column name mapped to point color.
#' @param col_palette Character vector of colors for the color scale; or
#'   \code{NULL} to use the plotly default. Defaults to \code{NULL}.
#' @param continuous Logical; if \code{TRUE}, treats \code{color_var} as a
#'   continuous scale; if \code{FALSE}, as a discrete factor. Defaults to
#'   \code{FALSE}.
#' @param x_lab Character; X-axis label. Defaults to \code{"x"}.
#' @param y_lab Character; Y-axis label. Defaults to \code{"y"}.
#' @param z_lab Character; Z-axis label. Defaults to \code{"z"}.
#' @param title Character; plot title. Defaults to \code{""}.
#' @param xrange Numeric vector of length 2 for X-axis limits, or \code{NULL}
#'   for automatic. Defaults to \code{NULL}.
#' @param yrange Numeric vector of length 2 for Y-axis limits, or \code{NULL}.
#'   Defaults to \code{NULL}.
#' @param zrange Numeric vector of length 2 for Z-axis limits, or \code{NULL}.
#'   Defaults to \code{NULL}.
#' @param eye Character preset (\code{"pos1"}, \code{"pos2"}, \code{"pos3"})
#'   or a named list with \code{x}, \code{y}, \code{z} for the camera
#'   position. Defaults to \code{"pos1"}.
#' @param zoom Numeric; camera zoom factor. Defaults to \code{1}.
#'
#' @return A \code{plotly} figure object.
#' @export
threeDplot <- function(data, x_var, y_var, z_var, color_var, col_palette = NULL, continuous = FALSE, x_lab = "x", y_lab = "y", z_lab = "z", title = "", xrange = NULL, yrange = NULL, zrange = NULL, eye = "pos1", zoom = 1){

  if(eye == "pos1"){
    eye = list(x = 0, y = 1.5, z = 0.8)
  }else if(eye == "pos2"){
    eye = list(x = 1.5, y = 0.3, z = 0.8)
  }else if(eye == "pos3"){
    eye = list(x = -1.5, y = 0.3, z = 0.8)
  }else if(length(eye != 3)){
    print("Camera values are not suitable, setting to default")
    eye = list(x = 0, y = 1.5, z = 0.8)
  }else{
    eye = eye
  }

  if(title == ""){
    title = paste0("3D plot of ")#, deparse(x_var), ", ", deparse(y_var), ", ", deparse(z_var))
  }

  if(is.null(col_palette)){
    colors = length(unique(data[,deparse(substitute(color_var))]))
  }

  # modify camera position according to zoom
  zoom = 1/zoom
  eye <- lapply(eye, function(x){x*zoom})

  # retrieve variable position
  x_pos <- which(colnames(data) == deparse(substitute(x_var)))[1]
  y_pos <- which(colnames(data) == deparse(substitute(y_var)))[1]
  z_pos <- which(colnames(data) == deparse(substitute(z_var)))[1]
  col_pos <- which(colnames(data) == deparse(substitute(color_var)))[1]
  data[, col_pos] <- as.factor(data[, col_pos])
  # setNames(col_palette, levels(data[, col_pos]))
  plotly <- plot_ly() %>%
    # Add scatter plot points
    add_trace(
      data = data,
      x = ~data[, x_pos],
      y = ~data[, y_pos],
      z = ~data[, z_pos],
      type = "scatter3d",
      mode = "markers",
      split = ~data[, col_pos],  # Ensures proper grouping by discrete variable
      marker = list( size = 3, opacity = 0.6#,
                     # color = ~data[, col_pos],  # Assign color based on a column
                     # colors = col_palette  # Apply custom colors
      ),
      showlegend = TRUE) %>%
    # Layout settings
    layout(
      scene = list(
        xaxis = list(title = x_lab, titlefont = list(size = 14)),
        yaxis = list(title = y_lab, titlefont = list(size = 14)),
        zaxis = list(title = z_lab, titlefont = list(size = 14)),
        camera = list(eye = eye)
      ),
      title = list(
        text = title,
        font = list(size = 16)
      )
    )

  if(!is.null(xrange)){
    plotly <- plotly %>% layout(scene = list(xaxis = list(range = xrange)))
  }
  if(!is.null(yrange)){
    plotly <- plotly %>% layout(scene = list(yaxis = list(range = yrange)))
  }
  if(!is.null(zrange)){
    plotly <- plotly %>% layout(scene = list(zaxis = list(range = zrange)))
  }
  return(plotly)
}

