# collection of functions to perform tests on data

#' Plot observed vs fitted time series for a single cell
#'
#' @param data List containing `traces`, `fit_traces`, and optionally `smooth_traces` (matrices: cells × timepoints).
#' @param ID Row index of the cell to plot.
#' @param title Plot title.
#' @param xlab,ylab Axis labels.
#' @param smooth Logical; overlay smoothed trace if available.
#' @param time_res Time resolution in hours.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
check_fit <- function(data, ID, title = "", xlab = "Time (h)", ylab = "Value",
                      smooth = TRUE, time_res = 0.5, ...) {

  len_data <- length(data$traces[ID, ])
  xvals <- seq(0, by = time_res, length.out = len_data)

  plot <- ggplot() +
    geom_line(aes(x = xvals, y = data$traces[ID, ]), color = "red") +
    geom_point(aes(x = xvals, y = data$fit_traces[ID, ]), color = "black") +
    labs(title = title, x = xlab, y = ylab) +
    scale_x_continuous(
      limits = c(0, len_data * time_res),
      breaks = seq(0, len_data * time_res, by = 24),
      minor_breaks = seq(12, len_data * time_res, by = 24)
    ) +
    theme_minimal()

  if (smooth && !is.null(data$smooth_traces)) {
    plot <- plot +
      geom_line(aes(x = xvals, y = data$smooth_traces[ID, ]), color = "blue")
  }

  return(plot)
}

#' Plot phase distribution across cells
#'
#' @param data Data frame with columns `ID` and `phase_var`.
#' @param phase_var Column name containing phase (in hours).
#' @param x_lab,ylab Axis labels.
#' @param xlim X-axis limits (default 0–24).
#' @param pt_size Point size (plotly only).
#' @param pt_color Point color.
#' @param plotly Logical; return interactive plot if TRUE.
#' @param multi Logical; overlay second dataset.
#' @param data2 Optional second dataset (same structure as `data`).
#' @param pt_color2 Color for second dataset (default "red").
#' @param alpha Point transparency (ggplot only).
#' @param ... Unused.
#'
#' @return A ggplot or plotly object.
#' @export
check_phase <- function(data, phase_var, x_lab = "Phase (h)", ylab = "Cell ID",
                        xlim = c(0, 24), pt_size = 6, pt_color = "black",
                        plotly = TRUE, multi = FALSE, data2 = NULL,
                        pt_color2 = "red", alpha = 1, ...) {

  data_x <- data[[phase_var]]

  if (plotly) {

    df_plot <- data.frame(ID = data$ID, phase = data_x)

    plot <- plot_ly(
      data = df_plot,
      x = ~phase,
      y = ~ID,
      type = "scatter",
      mode = "markers",
      text = ~paste0("Phase: ", round(phase, 2), "<br>ID: ", ID),
      hoverinfo = "text",
      marker = list(size = pt_size, color = pt_color)
    ) %>%
      layout(
        title = paste0(phase_var, " plot"),
        xaxis = list(title = x_lab, range = xlim,
                     tickvals = seq(min(xlim), max(xlim), by = 4)),
        yaxis = list(title = ylab)
      )

  } else {

    plot <- ggplot(data, aes(x = data_x, y = ID)) +
      geom_point(color = pt_color, alpha = alpha) +
      scale_x_continuous(
        limits = xlim,
        breaks = seq(min(xlim), max(xlim), by = 4)
      ) +
      scale_y_reverse() +
      labs(title = paste0(phase_var, " plot"), x = x_lab, y = ylab) +
      theme_minimal()

    mean_phase1 <- circular::mean.circular(
      circular::circular(data_x, units = "hour",
                         rotation = "clock", modulo = "asis"),
      na.rm = TRUE
    )

    plot <- plot +
      geom_vline(xintercept = mean_phase1, linetype = "dashed",
                 color = pt_color, linewidth = 1) +
      annotate("text", x = mean_phase1, y = min(data$ID),
               label = round(mean_phase1, 2), color = pt_color)

    if (multi && !is.null(data2)) {

      data_x2 <- data2[[phase_var]]

      plot <- plot +
        geom_point(data = data2,
                   aes(x = data_x2, y = ID),
                   color = pt_color2, alpha = alpha)

      mean_phase2 <- circular::mean.circular(
        circular::circular(data_x2, units = "hour",
                           rotation = "clock", modulo = "asis"),
        na.rm = TRUE
      )

      plot <- plot +
        geom_vline(xintercept = mean_phase2, linetype = "dashed",
                   color = pt_color2, linewidth = 1) +
        annotate("text", x = mean_phase2, y = min(data2$ID),
                 label = round(mean_phase2, 2), color = pt_color2)
    }
  }

  return(plot)
}