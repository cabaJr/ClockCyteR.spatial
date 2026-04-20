#' collection of funtions used in network analysis

#' Compute pairwise cell correlations and return edge list
#'
#' @param method Character; correlation method, either \code{"spearman"} or
#'   \code{"combined"} (Spearman + FFT-based rhythm similarity). Defaults to
#'   \code{"combined"}.
#' @param traces Numeric matrix of cell fluorescence traces (cells × frames).
#' @param network_dir Character; directory for saving correlation matrix plots.
#' @param filename Character; file identifier used in plot filenames.
#' @param interval_name Character; interval label used in plot filenames.
#'   Defaults to \code{"Full"}.
#' @param grid_coord Data frame of grid coordinates with columns \code{X} and
#'   \code{Y}, used for spatial weighting in \code{"combined"} mode.
#' @param cell_nodes Data frame of node metadata; must contain a \code{name}
#'   column matching row indices of \code{traces}.
#' @param plot_matrix Logical; whether to save a heatmap of the correlation
#'   matrix. Defaults to \code{TRUE}.
#' @param filter_nonrhythmic Logical; whether to remove edges with \code{NA}
#'   weights before returning. Defaults to \code{FALSE}.
#'
#' @return A data frame of edges with columns \code{from}, \code{to}, and
#'   \code{weight} (Fisher z-transformed correlation).
#' @export
traces_correlation <- function(
  method = "combined",
  traces,
  network_dir, #rm
  filename, #rm
  interval_name = "Full", #rm
  grid_coord,
  cell_nodes,
  plot_matrix = TRUE,
  filter_nonrhythmic = FALSE
) {

  if (method == "spearman") {
    cor_mat <- cor(t(traces), method = "spearman")

    z2 <- atanh(cor_mat)

    z_vals <- z2[upper.tri(z2, diag = FALSE)]
    z2[is.infinite(z2)] <- NA

    if (plot_matrix) {
      png(
        file.path(
          network_dir,
          paste0("Z2_correlation_matrix_", filename, "_", interval_name, ".png")
        ),
        width = 5000,
        height = 5000
      )
      heatmap(
        z2,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        col = scico(100, palette = "lajolla"),
        main = paste0(
          "Z2 Correlation matrix for ",
          filename,
          " - ",
          interval_name
        )
      )
      dev.off()
    }

    # create edges based on correlation threshold
    vals <- as.vector(z2)
    threshold <- quantile(z2, probs = 0.90, na.rm = TRUE)

    if(FALSE) { #TODO separate plotting from this function
    degree_correlation <- ggplot(data.frame(z_vals = z_vals), aes(x = z_vals)) +
      geom_histogram(bins = 120, fill = "darkseagreen", color = "black") +
      geom_vline(
        xintercept = threshold,
        color = "blue",
        linetype = "dashed",
        linewidth = 1.5
      ) +
      labs(
        title = "Degree weighted Distribution of atanh Spearman correlation",
        x = "Degree weighted",
        y = "Count"
      ) +
      theme_minimal() +
      scale_alpha_identity() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 20), # Increase y-axis title size
        axis.title.x = element_text(size = 20), # Increase x-axis title size
        axis.text.y = element_text(size = 18), # Increase y-axis label size
        axis.text.x = element_text(size = 18)
      )
    }

    high_corr <- which(z2 > threshold & upper.tri(z2), arr.ind = TRUE)

    edges_corr <- data.frame(
      from = cell_nodes$name[high_corr[, 1]],
      to = cell_nodes$name[high_corr[, 2]],
      weight = z2[high_corr]
    )
  } else if (method == "combined") {

    estimate_fft_phase_period <- function(
    x,
    dt = 0.5,
    min_h = 6,
    max_h = 36,
    pad_factor = 20          # how much to zero-pad
    ) {
      n <- length(x)

      # Zero padding
      n_pad <- n * pad_factor
      x_pad <- c(x, rep(0, n_pad - n))

      # FFT on padded trace
      fx <- fft(x_pad)
      power <- Mod(fx)^2

      # FFT frequency grid (cycles per hour)
      freqs_fft <- (0:(n_pad - 1)) / (n_pad * dt)

      # Keep positive FFT frequencies only
      sel <- 2:floor(n_pad / 2)
      freqs_fft <- freqs_fft[sel]
      power_fft <- power[sel]
      periods_fft <- 1 / freqs_fft

      # Restrict to [min_h, max_h]
      idx <- which(periods_fft >= min_h & periods_fft <= max_h)
      if (length(idx) < 2)
        return(list(period = NA, phase = NA, rel_power = NA))

      # Add dense frequency grid for interpolation
      freqs_dense <- 1 / seq(min_h, max_h, length.out = n)

      # Interpolate FFT power onto dense grid
      power_dense <- approx(freqs_fft, power_fft, xout = freqs_dense)$y

      # Find dominant period from combined grid
      dom_idx <- which.max(power_dense)
      dom_freq <- freqs_dense[dom_idx]
      dom_period <- 1 / dom_freq

      # Estimate phase from FFT coefficient
      # find nearest FFT bin to the dominant frequency
      nearest_fft_idx <- which.min(abs(freqs_fft - dom_freq))

      dom_phase <- Arg(fx[sel[nearest_fft_idx]])

      # Convert rad \u2192 hours
      phase_hours <- (dom_phase / (2*pi)) * dom_period
      phase_hours <- (phase_hours %% dom_period)

      # Rhythmicity strength
      rel_power <- power_dense[dom_idx] / sum(power_dense, na.rm = TRUE)

      list(
        period = dom_period,
        phase = phase_hours,
        rel_power = rel_power
      )
    }

    # Run for all traces
    dt <- 0.5  # 30 min sampling
    fft_stats <- apply(traces, 1, estimate_fft_phase_period, dt = dt)

    periods_est <- sapply(fft_stats, function(z) z$period)
    phases_est  <- sapply(fft_stats, function(z) z$phase)
    powers_est  <- sapply(fft_stats, function(z) z$rel_power)

    # Construct similarity matrices

    # phase similarity (circular)
    n_cells <- length(phases_est)
    phase_diff <- outer(1:n_cells, 1:n_cells, function(i,j){
      d <- abs(phases_est[i] - phases_est[j])
      Tavg <- (periods_est[i] + periods_est[j]) / 2
      pmin(d, Tavg - d)
    })

    phase_similarity <- exp(-(phase_diff^2) / (2 * 3^2))  # \u03c3 = 3h bandwidth

    # period similarity
    period_diff <- abs(outer(periods_est, periods_est, "-"))
    period_similarity <- exp(-(period_diff^2) / (2 * 2^2)) # \u03c3 = 2h

    # rhythmicity weighting (weak traces contribute less)
    rhythm_weight <- outer(powers_est, powers_est, "*")

    # final rhythm similarity matrix
    rhythm_similarity <- phase_similarity * period_similarity * rhythm_weight


    cor_spearman <- cor(t(traces), method = "spearman") #, use = "pairwise.complete.obs")

    fct_spe = 0.5
    fct_fft = 0.5

    combined_cor <- fct_spe * cor_spearman + fct_fft * rhythm_similarity
    if(FALSE){
    fft_traces <- t(apply(traces, 1, function(x) fft(x)))
    power_spectrum <- Mod(fft_traces)^2
    power_spectrum <- power_spectrum[, -1] # remove DC component

    N <- ncol(traces)
    freqs <- (1:(N / 2)) / N # use only positive frequencies
    periods <- 1 / freqs

    # select  ranges
    target_indices <- which(periods > 2 & periods < 36)
    selected_spectrum <- power_spectrum[, target_indices]

    #compute correlation between power spectra
    cor_fft <- cor(
      t(selected_spectrum),
      method = "pearson",
      use = "pairwise.complete.obs"
    )

    fct_spe = 0.5
    fct_fft = 0.5
    combined_cor <- fct_spe * cor_spearman + fct_fft * cor_fft
    combined_cor2 <- cor_spearman * cor_fft
}
    # coords: Nx2 matrix with x, y positions
    coordinates <- as.data.frame(grid_coord)[rownames(traces), c("X", "Y")]
    distance_matrix <- as.matrix(dist(coordinates)) # Euclidean distance between cells

    alpha <- 0.0001 # controls the spatial influence
    spatial_weight <- exp(-alpha * distance_matrix)

    combined_cor_spatial <- 0.99 * combined_cor + 0.01 * spatial_weight

    atanh_comb_cor <- atanh(combined_cor_spatial)
    atanh_comb_cor_clean <- atanh_comb_cor[upper.tri(
      atanh_comb_cor,
      diag = FALSE
    )]
    atanh_comb_cor[is.infinite(atanh_comb_cor)] <- NA

    if (plot_matrix) {
      png(
        file.path(
          network_dir,
          paste0(
            "combined_correlation_matrix_",
            filename,
            "_",
            interval_name,
            ".png"
          )
        ),
        width = 5000,
        height = 5000
      )
      heatmap(
        spatial_weight,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        col = scico(100, palette = "lajolla"),
        main = paste0(
          "Combined Correlation matrix for ",
          filename,
          " - ",
          interval_name
        )
      )
      dev.off()
    }

    # keep only top 20% of connections
    m <- as.matrix(atanh_comb_cor)
    m[lower.tri(m, diag = TRUE)] <- NA
    vals <- as.vector(m)
    vals <- vals[!is.na(vals)]
    threshold <- quantile(vals, probs = 0.90)
    high_corr <- which(m >= threshold, arr.ind = TRUE)

    if(FALSE) { #TODO separate plotting from this function
    degree_correlation <- ggplot(
      data.frame(atanh_comb_cor_clean = atanh_comb_cor_clean),
      aes(x = atanh_comb_cor_clean)
    ) +
      geom_histogram(bins = 120, fill = "darkseagreen", color = "black") +
      geom_vline(
        xintercept = threshold,
        color = "blue",
        linetype = "dashed",
        linewidth = 1.5
      ) +
      labs(
        title = "Degree weighted Distribution of atanh combined correlation (Spearman + FFT)",
        x = "Degree weighted",
        y = "Count"
      ) +
      theme_minimal() +
      scale_alpha_identity() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 20), # Increase y-axis title size
        axis.title.x = element_text(size = 20), # Increase x-axis title size
        axis.text.y = element_text(size = 18), # Increase y-axis label size
        axis.text.x = element_text(size = 18)
      )
    }

    edges_corr <- data.frame(
      from = cell_nodes$name[high_corr[, 1]],
      to = cell_nodes$name[high_corr[, 2]],
      weight = atanh_comb_cor[high_corr]
    )
  }
  # save rectangular plot
  if(FALSE) { #TODO separate plotting from this function
  savePlots(
    obj_to_save = list(
      degree_correlation = degree_correlation,
      degree_correlation = degree_correlation
    ),
    filename = filename,
    basepath = network_dir,
    extension = "svg",
    p.width = 2800,
    p.height = 2000
  )
  }
  # remove NA from edges
  if(filter_nonrhythmic){
  edges_corr <- edges_corr[complete.cases(edges_corr), ]
  }

  return(edges_corr)
}


#' Reorder and relabel network clusters
#'
#' @param graph_obj An igraph object with vertex attribute \code{module}.
#' @param node_data Data frame of node metadata including columns \code{name},
#'   \code{cluster}, and \code{phase_circ}.
#' @param cluster_order Character; ordering criterion, either \code{"size"}
#'   (decreasing) or \code{"phase"} (ascending mean phase). Defaults to
#'   \code{"size"}.
#' @param network_dir Character; directory used for saving cluster outputs.
#' @param interval_name Character; interval label.
#' @param cluster_threshold Integer; clusters with this many cells or fewer are
#'   collapsed into \code{"NULL"}. Defaults to \code{3}.
#'
#' @return A named list with elements \code{g} (updated igraph object),
#'   \code{node_data} (updated data frame with relabelled \code{cluster}
#'   factor), and \code{mean_phase_per_cluster} (data frame or \code{NULL}).
#' @export
order_clusters <- function(graph_obj,
                          node_data,
                          cluster_order = "size",
                          network_dir,
                          interval_name = int_list$name,
                          cluster_threshold = 3
                          ) {

  node_data$cluster <- as.character(node_data$cluster)

  # Collapse small clusters (size <= 2) into "NULL"
  cluster_counts <- table(node_data$cluster)
  singleton_cluster <- names(cluster_counts[cluster_counts <= cluster_threshold])
  node_data$cluster[node_data$cluster %in% singleton_cluster] <- "NULL"

  # list clusters by size and re-index
  node_data$cluster <- as.character(node_data$cluster)
  non_null <- node_data$cluster != "NULL"

  # Count sizes
  cluster_sizes <- table(node_data$cluster[non_null])
  if (sum(as.numeric(non_null)) > 0) {

    if (cluster_order == "size") {
    # Order by decreasing size
    sorted_clusters <- names(sort(cluster_sizes, decreasing = TRUE))

    # Create mapping with sequence based on new clusters
    new_labels <- setNames(
      as.character(seq_along(sorted_clusters)),
      sorted_clusters
    )

    node_data$cluster[non_null] <- new_labels[node_data$cluster[non_null]]

    # Relabel non-NULL clusters
    ordered_levels <- c(as.character(seq_along(sorted_clusters)), "NULL")
    node_data$cluster <- factor(node_data$cluster, levels = ordered_levels)

    # match the clusters in the igraph
    V(graph_obj)$module <- node_data$cluster[match(V(graph_obj)$name, node_data$name)]

    mean_phase_per_cluster <- node_data %>%
      filter(cluster != "NULL") %>%
      group_by(cluster) %>%
      dplyr::summarize(mean_phase = as.numeric(mean(phase_circ, na.rm = TRUE)))

  } else if (cluster_order == "phase") {

    #  Compute mean phase per cluster (excluding "NULL")
    mean_phase_per_cluster <- node_data %>%
      filter(cluster != "NULL") %>%
      group_by(cluster) %>%
      dplyr::summarize(mean_phase = as.numeric(mean(phase_circ, na.rm = TRUE))) %>%
      arrange(mean_phase) # ascending order of phase; use desc(mean_phase) for descending

    if(FALSE) { # TODO remove from here and save outside
    # save as table
    write.csv(
      mean_phase_per_cluster,
      file = file.path(
        network_dir,
        paste0("mean_phase_per_cluster_", filename, "_", interval_name, ".csv")
      ),
      row.names = FALSE
    )
    }

    # Create mapping from old cluster IDs to new sequential IDs
    sorted_clusters <- mean_phase_per_cluster$cluster
    new_labels <- setNames(
      as.character(seq_along(sorted_clusters)),
      sorted_clusters
    )

    # Reassign cluster labels based on mean phase
    non_null <- node_data$cluster != "NULL"
    node_data$cluster[non_null] <- new_labels[node_data$cluster[non_null]]

    # Set factor levels (ordered by mean phase)
    ordered_levels <- c(as.character(seq_along(sorted_clusters)), "NULL")
    node_data$cluster <- factor(node_data$cluster, levels = ordered_levels)

    # Update igraph object
    V(graph_obj)$module <- node_data$cluster[match(V(graph_obj)$name, node_data$name)]
  }
  } else {
    mean_phase_per_cluster <- NULL
  }

  return(list(g = graph_obj,
              node_data = node_data,
              mean_phase_per_cluster = mean_phase_per_cluster)
         )
}


#' Doughnut chart of cluster abundance
#'
#' @param node_data Data frame with a \code{cluster} column.
#' @param colors_map Data frame with columns \code{cluster} and \code{color},
#'   as returned by \code{assign_cluster_colors()}.
#'
#' @return A \code{ggplot} object.
#' @export
doughnut_plot <- function(node_data, colors_map) {

  # Ensure factor with full set of levels
  node_data$cluster <- factor(node_data$cluster, levels = colors_map$cluster)

  # Create frequency table that keeps missing levels
  cluster_df <- as.data.frame(table(node_data$cluster))
  colnames(cluster_df) <- c("cluster", "n")

  # Compute percentages safely even if n = 0
  cluster_df$percentage <- cluster_df$n / sum(cluster_df$n) * 100

  # Create the donut plot
  plot <- ggplot(cluster_df, aes(x = 2, y = percentage, fill = cluster)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) + # Creates the hole in the middle
    scale_fill_manual(values = colors_map$color) +
    theme_void() +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 12)
    ) +
    labs(fill = "Cluster", title = "Cluster Abundance")

  return(plot)
}

#' Spatial scatter plot of cluster assignments
#'
#' @param node_data Data frame with columns \code{X}, \code{Y}, and
#'   \code{cluster}.
#' @param colors Character vector of colors, named by cluster level.
#' @param filename Character; used in the plot title.
#'
#' @return A \code{ggplot} object.
#' @export
spatial_clusters_plot <- function(node_data, colors, filename) {
  plot <- ggplot(node_data, aes(x = X, y = Y, color = factor(cluster))) +
    geom_point(size = 2, shape = 15) +
    scale_color_manual(values = colors, name = "Cluster") +
    coord_fixed() +
    scale_y_reverse() +
    theme_minimal()+
    labs(
      title = paste0(filename, " - Clusters"),
      x = "Width (\u00b5m)",
      y = "Height (\u00b5m)",
    ) +
    theme(plot.title = element_text(size = 11),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 15))

  return(plot)
}

#' Histogram or bar chart of a node variable
#'
#' @param var Numeric vector to plot; \code{NA}s are removed.
#' @param bin_size Integer; number of bins for histogram mode. Defaults to
#'   \code{30}.
#' @param bin_color Character; fill color for bars.
#' @param x_label Character; x-axis label.
#' @param y_label Character; y-axis label. Defaults to \code{"Count"}.
#' @param title Character; plot title. Defaults to \code{""}.
#' @param mode Character; \code{"hist"} for histogram or \code{"bar"} for bar
#'   chart. Defaults to \code{"hist"}.
#' @param ... Optional arguments: \code{xlims} (numeric vector of length 3:
#'   min, max, step) to set axis limits and breaks; \code{mean} (any value) to
#'   add a vertical mean line.
#'
#' @return A \code{ggplot} object.
#' @export
hist_plot <- function(
    var,
    bin_size = 30,
    bin_color,
    x_label,
    y_label = "Count",
    title = "",
    mode = "hist",
    ...
) {
  args <- list(...)

  data <- data.frame(var = {{var[complete.cases(var)]}})

  plot <- ggplot(data, aes(x = var))

  if(mode == "hist"){
    plot <- plot +
      geom_histogram(bins = bin_size, fill = bin_color, color = "black")
  }
  else if(mode == "bar"){
    plot <- plot+
      geom_bar(fill = bin_color, color = "black", stat = "count")
  }

    plot <- plot +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()

  if(!is.null(args$xlims)) {
    lims <- c((args$xlims[1]), (args$xlims[2]))
    breaks <- seq(args$xlims[1], args$xlims[2], args$xlims[3])
    min_breaks <- seq((args$xlims[1]+(args$xlims[3]/2)),
                      (args$xlims[2]-(args$xlims[3]/2)),
                      (args$xlims[3]/2))

  suppressWarnings(
  plot <- plot +
    scale_x_continuous(limits = lims,
                       breaks = breaks,
                       minor_breaks = min_breaks
    )
  )
  }

  if(!is.null(args$mean)){
    plot <- plot +
      geom_vline(aes(xintercept = mean(var, na.rm = TRUE)))

  }

  return(plot)
}

# PLOTTING ####

#' Assign colors to clusters
#'
#' @param node_data Data frame with a \code{cluster} factor column.
#'
#' @return A data frame with columns \code{cluster} and \code{color}.
#'   \code{"NULL"} clusters are assigned \code{"grey80"}.
#' @export
assign_cluster_colors <- function(node_data){

  colors <- c('#3750db','#f47a22','#29bdab','#991919','#3998f5','#228c68',
              '#b732cc','#fcff5d','#772b9d','#0ec434','#f07cab','#277da7',
              '#d30b94','#7dfc00','#e68f66','#235b54','#946aa2','#f22020',
              '#8ad8e8','#37294f','#96341c','#ffc413','#c3a5b4','#c56133',
              '#5d4c86','#ffcba5','#632819','#201923','#1dfff3','#8c6dfd')

  cluster_levels <- levels(node_data$cluster)
  cluster_levels_no_null <- cluster_levels[cluster_levels != "NULL"]
  n_clusters <- length(cluster_levels_no_null)
  if (n_clusters > length(colors)) {
    message("Not enough colors in palette for the number of clusters. Some clusters will share the same color")
    rep_n <- n_clusters %/% 30

    colors_many <- rep(colors, times = (rep_n +1))
    cluster_colors <- colors_many[1:n_clusters]

  }else if (n_clusters == 0) {
    cluster_colors <- NULL
  } else {
    cluster_colors <- colors[1:n_clusters]
  }

  cluster_colors <- as.character(cluster_colors)
  names(cluster_colors) <- cluster_levels_no_null

  # Add gray for NULL
  cluster_colors["NULL"] <- "grey80"
  if(is.null(cluster_levels_no_null)) cluster_levels <- "NULL"
  color_map = data.frame(cluster = cluster_levels,
                         color = cluster_colors)
  return(color_map)
}

#' Render an igraph network with one of several preset styles
#'
#' @param igraph_obj An igraph object to plot.
#' @param color_map Data frame with columns \code{cluster} and \code{color}, as
#'   returned by \code{assign_cluster_colors()}.
#' @param type Character; plot style. One of \code{"Vertex_edges"},
#'   \code{"Vertex"}, \code{"Vertex_edges_empty"}, \code{"Edges"},
#'   \code{"Clusters_skeleton"}, or \code{"Clusters_connected"}.
#' @param layout_list List of layout components as returned by
#'   \code{network_plot_layout()}.
#' @param network_dir Character; directory where SVG files are saved.
#' @param width Numeric; SVG device width in inches. Defaults to \code{6}.
#' @param height Numeric; SVG device height in inches. Defaults to \code{8}.
#' @param plot_suffix Character; optional suffix appended to the output
#'   filename. Defaults to \code{""}.
#' @param membership_vec Integer vector mapping each vertex to a cluster, used
#'   for \code{"Clusters_skeleton"} and \code{"Clusters_connected"} types.
#'   Defaults to \code{NULL}.
#' @keywords internal
network_plot <- function(igraph_obj,
                        color_map,
                        type, layout_list,
                        network_dir,
                        width = 6,
                        height = 8,
                        plot_suffix = "",
                        membership_vec = NULL
                        )
  {

  # browser()
  cluster_colors <- color_map$color
  names(cluster_colors) <- color_map$cluster

  igraph_table <- as_tbl_graph(igraph_obj)
  module_ids <- sort(unique(V(igraph_table)$module))
  legend_colors <- cluster_colors[as.character(module_ids)]

  if(plot_suffix != ""){
    plot_suffix <- paste0("_", plot_suffix)
  }
  # Plot according to type
  switch(type,
         "Vertex_edges" = {
           svg(file.path(network_dir, paste("Vert_Edges", plot_suffix,".svg")), width = 6, height = 8)
           plot(
             igraph_obj,
             layout = layout_list$layout_xy,
             vertex.label = NA,
             vertex.size = V(igraph_obj)$phase_crn * 9,
             vertex.color = layout_list$vertex_color_mapped_transp,
             vertex.frame.color = NA,
             edge.color = layout_list$edge_colors,
             edge.width = 0.5,
             asp = layout_list$xy_ratio
           )

           # Add legend
           legend(
             "topright",  # or use numeric coords like c(1, 1)
             legend = module_ids,
             col = legend_colors,
             pch = 16,
             pt.cex = 2,
             bty = "n",
             title = "Module"
           )

           # Size legend for phase coherence
           # Select representative phase coherence values
           phase_vals <- c(min(V(igraph_table)$phase_crn, na.rm = TRUE),
                           max(V(igraph_table)$phase_crn, na.rm = TRUE)/2,
                           max(V(igraph_table)$phase_crn, na.rm = TRUE))

           size_vals <- phase_vals   # Same scaling as vertex.size

           cex_vals <- scales::rescale((size_vals), to = c(0.1, 1.5))

           legend(
             "bottomright",  # adjust position as needed
             legend = sprintf("%.1f", phase_vals),
             pt.cex = cex_vals,  # convert to 'cex' scale for consistency
             pch = 16,
             col = "gray30",
             bty = "n",
             title = "Phase \nCoherence"
           )
           dev.off()
         },
         "Vertex" = {
           svg(file.path(network_dir, paste("Vertex", plot_suffix,".svg")), width = 6, height = 8)
           plot(
             igraph_table,
             layout = layout_list$layout_xy,
             vertex.label = NA,
             vertex.size =  scales::rescale(V(igraph_table)$phase_crn, to = c(1, 6.5)),
             vertex.color = layout_list$vertex_color_mapped,
             vertex.frame.color = NA,
             edge.color = NA,
             asp = layout_list$xy_ratio
           )
           # Add legend (adjust 'x' and 'y' as needed)
           legend(
             "topright",  # or use numeric coords like c(1, 1)
             legend = module_ids,
             col = legend_colors,
             pch = 16,
             pt.cex = 2,
             bty = "n",
             title = "Module"
           )
           dev.off()
         },
         "Vertex_edges_empty" = {
           svg(file.path(network_dir, paste("Vert_empty_edges", plot_suffix,".svg")), width = 6, height = 8)
           plot(
             igraph_table,
             layout = layout_list$layout_xy,
             vertex.label = NA,
             vertex.size = 9,
             vertex.color = NA,
             vertex.frame.color = scales::alpha("black", alpha = 0.2),
             edge.color = layout_list$edge_colors,
             edge.width = 0.5,
             asp = layout_list$xy_ratio
           )
           dev.off()
         },
         "Edges" = {
           svg(file.path(network_dir, paste("Edges", plot_suffix,".svg")), width = 6, height = 8)
           plot(
             igraph_table,
             layout = layout_list$layout_xy,
             vertex.label = NA,
             vertex.size = 0,
             vertex.color = NA,
             vertex.frame.color = NA,
             edge.color = layout_list$edge_colors,
             edge.width = 0.5,
             asp = layout_list$xy_ratio
           )
           dev.off()
         },
         "Clusters_skeleton" = {
           # plot with only edges connecting clusters

           igraph_clustered <- contract(igraph_table, membership_vec)
           # Simplify multiple edges into one weighted edge
           igraph_meta <- simplify(igraph_clustered, edge.attr.comb = list(weight="sum"))
           layout_meta <- rowsum(layout_list$layout_xy, membership_vec) / as.vector(table(membership_vec))

           svg(file.path(network_dir, paste("Clusters_skeleton", plot_suffix,".svg")), width = 6, height = 8)
           plot(igraph_meta,
                layout = layout_meta,
                vertex.label = NA,
                vertex.size =  6,
                vertex.color = NA,
                asp = layout_list$xy_ratio,
                edge.width= scales::rescale(E(igraph_meta)$weight, to = c(1, 5)), vertex.size=10,
                edge.color = adjustcolor("black", alpha.f = 0.8),
           )
           dev.off()

         },
         "Clusters_connected" = {
           igraph_clustered <- contract(igraph_table, membership_vec)
           # Simplify multiple edges into one weighted edge
           igraph_meta <- simplify(igraph_clustered, edge.attr.comb = list(weight="sum"))
           layout_meta <- rowsum(layout_list$layout_xy, membership_vec) / as.vector(table(membership_vec))

           svg(file.path(network_dir, paste("Clusters_connected", plot_suffix,".svg")), width = 6, height = 8)

           ## Base layer: all vertices, no edges
           plot(
             igraph_table,
             layout = layout_list$layout_xy,
             vertex.label = NA,
             vertex.size = 6.5,
             vertex.color = adjustcolor(layout_list$vertex_color_mapped, alpha.f = 0.85),
             vertex.frame.color = NA,
             edge.color = NA,      # suppress edges
             asp = layout_list$xy_ratio
           )

           ##  Overlay layer: cluster\u2013cluster edges
           plot(
             igraph_meta,
             layout = layout_meta,
             add = TRUE,
             vertex.label = NA,
             vertex.size = 1,
             vertex.frame.color = NA,
             vertex.color = NA,
             asp = layout_list$xy_ratio,
             edge.width = scales::rescale(E(igraph_meta)$weight, to = c(1, 8)),
             edge.color = adjustcolor("black", alpha.f = 0.6)
           )

           legend(
             "topright",  # or use numeric coords like c(1, 1)
             legend = module_ids,
             col = legend_colors,
             pch = 16,
             pt.cex = 2,
             bty = "n",
             title = "Cluster"
           )

           dev.off()
         }
         )

}

# SHORT Functions ####

#' Calculate statistics on each cluster
#'
#' @param node_data table containing period analysis in each node
#' @keywords internal
cluster_stats <- function(node_data) {

  # variance of cluster phases
  mean_cluster_phases <- node_data %>%
    group_by(cluster) %>%
    summarise(
      mean_phase = as.numeric(mean(phase_norm, na.rm = TRUE))
    )
  # filter out NULL cluster and calcuate the variance on the remaining mean phases
  variance_cluster_phases <- mean_cluster_phases %>%
    filter(cluster != "NULL") %>%
    summarise(
      variance = var(mean_phase, na.rm = TRUE)
    )

  # variance of cellular phases in each cluster
  cluster_phases_variance <- node_data %>%
    group_by(cluster) %>%
    summarise(
      phase_variance = var(phase_norm, na.rm = TRUE)
    )

  cluster_stats <- list(mean_cluster_phases = mean_cluster_phases,
                        variance_cluster_phases = variance_cluster_phases,
                        cluster_phases_variance = cluster_phases_variance)

  return(cluster_stats)
}

#' calculate node metrics
#'
#' @param g igraph object
#' @keywords internal

node_metrics <- function(g){
  # compute degree (i.e. the number of connections each node has)
  deg <- degree(g, mode = "all")
  str <- strength(g, mode = "all", weights = E(g)$weight)
  deg_w <- str / deg

  node_metrics <- data.frame(
    name = as.integer(V(g)$name),
    degree = deg,
    strength = str,
    mean_edge_weight = deg_w
  )
  return(node_metrics)
}

#' summarise node data
#'
#' @param node_data Data frame of node-level data including columns
#'   \code{cluster}, \code{phase_crn}, \code{period_crn}, \code{amplitude},
#'   \code{period}, \code{clust_coef}, \code{degree}, and \code{strength}.
#' @keywords internal
summarise_cluster_data <- function(node_data){
  tbl <- node_data %>%
    group_by(cluster) %>%
    summarise(
      cluster_phase_crn = round(mean(phase_crn, na.rm = TRUE), 3),
      cluster_period_crn = round(mean(period_crn, na.rm = TRUE), 3),
      cluster_amplitude = round(mean(amplitude, na.rm = TRUE), 2),
      cluster_period = round(mean(period, na.rm = TRUE), 2),
      #cluster_similarity = round(mean(similarity, na.rm = TRUE), 2),
      cluster_coeff = round(mean(clust_coef, na.rm = TRUE), 2),
      #cluster_CI_score = round(mean(CI_score, na.rm = TRUE), 2),
      cluster_degree = round(mean(degree, na.rm = TRUE), 2),
      cluster_strength = round(mean(strength, na.rm = TRUE), 2),
      size = n(),
      percentage = round((n() / nrow(node_data) * 100), 2)
    )
  return(tbl)
}

#' Filter network edges or vertices belonging to NULL clusters
#'
#' @param igraph_obj An igraph object with vertex attribute \code{module}.
#' @param delete.edges Logical; if \code{TRUE}, removes vertices in
#'   \code{"NULL"} clusters entirely; if \code{FALSE}, removes only edges
#'   connecting to \code{"NULL"} vertices. Defaults to \code{TRUE}.
#'
#' @return A filtered igraph object.
filter_edges <- function(igraph_obj, delete.edges = TRUE){

  if (delete.edges) {

    valid_vertex <- !is.na(V(igraph_obj)$module) & V(igraph_obj)$module != "NULL"

    induced_subgraph(igraph_obj, vids = V(igraph_obj)[valid_vertex])

  } else {
  # Logical vector per vertex
  valid_vertex <- !is.na(V(igraph_obj)$module) & V(igraph_obj)$module != "NULL"

  # Get edge list as vertex indices (fast)
  el <- ends(igraph_obj, E(igraph_obj), names = FALSE)

  # Keep edges where BOTH endpoints are valid
  keep_edges <- valid_vertex[el[,1]] & valid_vertex[el[,2]]

  subgraph_from_edges(igraph_obj,
                      E(igraph_obj)[keep_edges],
                      delete.vertices = FALSE)
  }
}

#' Prepare layout components for network plotting
#'
#' @param igraph_obj An igraph object whose vertex names correspond to rows in
#'   \code{channel_nodes}.
#' @param channel_nodes Data frame of node metadata with columns \code{X},
#'   \code{Y}, and \code{module} (or matching vertex attributes).
#' @param color_map Data frame with columns \code{cluster} and \code{color}, as
#'   returned by \code{assign_cluster_colors()}.
#' @keywords internal
network_plot_layout <- function(igraph_obj, channel_nodes, color_map){
  cluster_colors <- color_map$color
  names(cluster_colors) <- color_map$cluster

  igraph_table <- tidygraph::as_tbl_graph(igraph_obj)
  # Prepare layout (invert Y)
  layout_xy <- as.matrix(channel_nodes[, c("X", "Y")])
  layout_xy[, 2] <- -layout_xy[, 2]

  layout_xy_df <- as.data.frame(layout_xy)
  colnames(layout_xy_df) <- c("X", "Y")
  xy_ratio = diff(range(layout_xy_df$Y)) / diff(range(layout_xy_df$X))

  vertex_color_mapped <- cluster_colors[as.character(V(igraph_table)$module)]
  vertex_color_mapped_transp <- scales::alpha(
    cluster_colors[as.character(V(igraph_table)$module)],
    alpha = 0.8)

  edge_alpha <- scales::rescale(E(igraph_table)$weight, to = c(0.02, 0.95))
  edge_colors <- rgb(0.1, 0.1, 0.1, alpha = edge_alpha)

  layout_list <- list(
    layout_xy = layout_xy,
    xy_ratio = xy_ratio,
    vertex_color_mapped = vertex_color_mapped,
    vertex_color_mapped_transp = vertex_color_mapped_transp,
    edge_colors = edge_colors
  )
  return(layout_list)
}

# UTILS ####

#' Assign analysis time window from file length
#'
#' @param time_wind Logical; if \code{FALSE}, the full recording length is used
#'   as the interval. If \code{TRUE}, the pre-defined \code{intervals} object
#'   in the calling environment is returned unchanged.
#' @param foldername Character; path to the folder containing grid-value CSV
#'   files.
#' @param time_res Numeric; temporal resolution in hours per frame. Defaults to
#'   \code{0.5}.
#' @param Channel1_an Logical; if \code{TRUE}, reads from the channel-1 grid
#'   file; otherwise reads from channel 2. Defaults to \code{FALSE}.
#' @keywords internal
assign_time_wind <- function(time_wind, foldername, time_res = 0.5, Channel1_an = FALSE){
  # TODO add checks if paths return empty file
  # assign full time window if subsetting is not selected
  if (!time_wind){
    if(Channel1_an){
      Ch1_grid_vals_path = file.path(foldername, "ch1_1_grid_vals.csv")
      Ch1_grid_vals = importGridVals(Ch1_grid_vals_path)
      intervals = list("Full" = c(0, time_res*length(Ch1_grid_vals[1, ])))
    }else{
      Ch2_grid_vals_path = file.path(foldername, "ch2_2_grid_vals.csv")
      Ch2_grid_vals = importGridVals(Ch2_grid_vals_path)
      intervals = list("Full" = c(0, time_res*length(Ch2_grid_vals[1, ])))
    }
  }else{intervals = intervals}
  return(intervals)
}

#' Create a subdirectory for an analysis interval
#'
#' @param int Named list element representing a single interval; the name is
#'   \code{"Full"} for whole-recording analyses or an arbitrary label for
#'   sub-intervals, and the value is a numeric vector \code{c(start, end)}.
#' @param foldername Character; base directory in which the interval
#'   subdirectory will be created.
#' @keywords internal
create_int_fold <- function(int, foldername){
  if(names(int) == "Full"){
    interval_name = "Full"
  } else {
    interval_name <- paste(int[[1]][1], int[[1]][2], sep = "_")
  }

  int_fold <- create_folder(foldername, interval_name)
  return(list(folder = int_fold,
              name = interval_name)
         )
}
