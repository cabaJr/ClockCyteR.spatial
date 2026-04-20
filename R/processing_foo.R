# collection of functions to process data

# function to align phases to a standard
#' Align a vector of phases to a target Circadian Time
#'
#' @param data Numeric vector of phases in radians.
#' @param CT Numeric; target Circadian Time in hours. Defaults to \code{6}.
#' @param ... Currently unused.
#'
#' @return Numeric vector of aligned phases in radians.
#' @keywords internal
align_phase <- function(data, CT = 6, ...) {
  phases = data
  mean_phase = mean(phases, na.rm = TRUE)
  CT = CT
  radCT = (CT / 12) * pi
  difference = mean_phase - radCT
  if (radCT >= mean_phase) {
    phases_aligned = phases + difference
  } else {
    phases_aligned = phases - difference
  }
  return(phases_aligned)
}


# Function to calculate AUC using the trapz function
#' Calculate the area under a fluorescence trace
#'
#' @param y Numeric vector of fluorescence values.
#' @param normalize Logical; if \code{TRUE}, divides the AUC by the number of
#'   time points. Defaults to \code{TRUE}.
#'
#' @return Numeric; the (optionally normalised) trapezoidal AUC.
#' @keywords internal
calculate_auc <- function(y, normalize = TRUE) {
  # Trapezoidal method
  time <- seq(0, length.out = length(y), by = 0.5)
  auc_trapz <- trapz(time, y)
  if (normalize) {
    auc_trapz <- auc_trapz / length(time)
  }

  return(AUC1 = auc_trapz)
}

#' Compute circular statistics from a period table
#'
#' @param period_table Data frame of period-analysis results; must contain a
#'   \code{phase_rad} column.
#'
#' @return A named list with elements \code{mean_phase}, \code{RT} (Rayleigh
#'   test result), \code{vectorLength}, \code{circStats} (Rayleigh p-value),
#'   and \code{phase_var} (angular variance).
#' @keywords internal
circular_stats <- function(period_table) {
  period_table_clean <- period_table[complete.cases(period_table),]
  phases <- period_table_clean$phase_rad
  # Compute and plot mean direction
    # Calculate the mean circular statistic
    suppressWarnings(
    meanTest <- circular::mean.circular(phases,
                                        Rotation = "counter",
                                        na.rm = TRUE))

    # Perform a Rayleigh test on RCaMP
    suppressWarnings(
    RT <- rayleigh.test(phases))

    # Calculate the vector length statistic
    vectorLength <- RT$statistic[1]

    # Get the p-value from the Rayleigh test
    circStats <- RT$p.value[1]

    # Round the p-value to 3 decimal places
    try(circStats <- round(circStats, digits = 3),
        silent = TRUE,
        circStats <- NA
        )


    # Angular variance
    variance <- circular::angular.variance(phases, na.rm = TRUE)

  list(mean_phase = meanTest,
       RT = RT,
       vectorLength = vectorLength,
       circStats = circStats,
       phase_var = variance)
}

#' Composite Interaction Score (CI) between two variables
#'
#' @param tbl1 Data frame containing the first variable and an \code{ID}
#'   column.
#' @param var1 Character; column name of the first variable in \code{tbl1}.
#' @param tbl2 Data frame containing the second variable.
#' @param var2 Character; column name of the second variable in \code{tbl2}.
#' @param weight Logical; if \code{TRUE}, weights the score by
#'   \code{1 - |var1 - var2|}. Defaults to \code{FALSE}.
#' @param merge Logical; reserved for future use. Defaults to \code{FALSE}.
#'
#' @return A data frame with columns \code{ID} and \code{CI_score}.
#' @keywords internal
CI_score <- function(tbl1, var1, tbl2, var2, weight = FALSE, merge = FALSE) {
  # TODO add NA vals filtering
  score <- sqrt(tbl1[var1] * tbl2[var2])
  if (weight) {
    weight = 1 - abs(tbl1[var1] - tbl2[var2])
    score <- score * data$weight
  }

  CI_results <- data.frame(ID = tbl1$ID, CI_score <- score) %>%
    `colnames<-`(c("ID", "CI_score"))

  return(CI_results)
}

#function to compute spatial segregation index
#' Compute a spatial segregation index across clusters
#'
#' @param node_data Data frame of node metadata with columns \code{cluster},
#'   \code{X}, and \code{Y}.
#' @param min_cluster_size Integer; clusters with fewer cells than this are
#'   excluded before computing the index. Defaults to \code{3}.
#'
#' @return A named list with elements \code{global_index} (numeric) and
#'   \code{overlap_matrix} (square matrix of pairwise cluster overlaps).
#' @keywords internal
compute_spatial_segregation <- function(node_data, min_cluster_size = 3) {

  node_data <- node_data[which(node_data$cluster != "NULL"), ]
  if(nrow(node_data) > 0) {
  node_data$cluster <- droplevels(node_data$cluster)
  }
  # Filter out small clusters if needed
  cluster_counts <- table(node_data$cluster)
  keep_clusters <- names(cluster_counts[cluster_counts >= min_cluster_size])
  node_data <- node_data[node_data$cluster %in% keep_clusters, ]

  if (length(unique(node_data$cluster)) < 2) {
    warning("Less than 2 clusters remaining after filtering")
    return(list(global_index = NA, overlap_matrix = NULL))
  }

  # Convert to sf
  points_sf <- st_as_sf(node_data, coords = c("X", "Y"))

  # Compute convex hulls
  hulls <- points_sf |>
    group_by(cluster) |>
    summarise(geometry = st_combine(geometry), .groups = "drop") |>
    mutate(geometry = st_convex_hull(geometry))

  n <- nrow(hulls)
  overlap_matrix <- matrix(
    NA,
    nrow = n,
    ncol = n,
    dimnames = list(hulls$cluster, hulls$cluster)
  )

  # Pairwise segregation
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      inter <- st_intersection(
        st_geometry(hulls[i, ]),
        st_geometry(hulls[j, ])
        )
      if (length(inter) > 0) {
        area_inter <- st_area(inter)
        area_union <- st_area(st_union(
          st_geometry(hulls[i, ]),
          st_geometry(hulls[j, ]))
        )
        segregation <- 1 - (as.numeric(area_inter) / as.numeric(area_union))
      } else {
        segregation <- 1
      }
      overlap_matrix[i, j] <- segregation
      overlap_matrix[j, i] <- segregation
    }
  }

  # Cluster sizes for weighting
  cluster_sizes <- table(node_data$cluster)
  weights <- outer(cluster_sizes, cluster_sizes)
  weights <- weights[upper.tri(weights)]

  # Global weighted segregation
  seg_values <- overlap_matrix[upper.tri(overlap_matrix, diag = FALSE)]
  global_segregation <- weighted.mean(seg_values, weights, na.rm = TRUE)

  return(list(
    global_index = global_segregation,
    overlap_matrix = overlap_matrix
  ))
}
# foo to analyse TS and make circular plot
#' Run FFT-NLLS period analysis on a fluorescence trace matrix
#'
#' @param df Numeric matrix or data frame of fluorescence traces (cells ×
#'   frames), or a long-format table if \code{prep_tbl = FALSE}.
#' @param excludeNC Logical; if \code{TRUE}, excludes non-circadian cells
#'   from the returned table. Defaults to \code{FALSE}.
#' @param top Numeric; upper period bound in hours. Defaults to \code{30}.
#' @param bottom Numeric; lower period bound in hours. Defaults to
#'   \code{18}.
#' @param save.trace Logical; whether to store individual fitted traces.
#'   Defaults to \code{FALSE}.
#' @param rm.start Integer; number of initial time points to discard before
#'   analysis. Defaults to \code{0}.
#' @param prep_tbl Logical; if \code{TRUE}, calls \code{prep_table()} to
#'   reshape \code{df} before analysis. Defaults to \code{TRUE}.
#' @param preprocess Logical; if \code{TRUE}, smooths and detrends traces
#'   during table preparation. Defaults to \code{TRUE}.
#' @param transp Logical; if \code{TRUE}, transposes \code{df} during table
#'   preparation. Defaults to \code{TRUE}.
#' @param add_t Logical; if \code{TRUE}, adds a time column during table
#'   preparation. Defaults to \code{TRUE}.
#' @param time_res Numeric; temporal resolution in hours per frame. Defaults
#'   to \code{0.5}.
#' @param smooth Logical; if \code{TRUE}, uses the smoothed trace for
#'   analysis. Defaults to \code{TRUE}.
#' @param invert Logical; if \code{TRUE}, inverts the fluorescence signal
#'   before analysis. Defaults to \code{FALSE}.
#' @param filterRAE Numeric threshold for the Relative Amplitude Error;
#'   cells with RAE above this value are excluded. Pass \code{NULL} to
#'   disable filtering. Defaults to \code{NULL}.
#' @param ... Currently unused.
#'
#' @return A named list with elements \code{period_table} (data frame of
#'   per-cell results), \code{period_table_unfiltered}, and optionally
#'   \code{traces} and \code{fit_traces}.
#' @export
computePeriod <- function(
  df,
  excludeNC = FALSE,
  top = 30,
  bottom = 18,
  save.trace = FALSE,
  rm.start = 0,
  prep_tbl = TRUE,
  preprocess = TRUE,
  transp = TRUE,
  add_t = TRUE,
  time_res = 0.5,
  smooth = TRUE,
  invert = FALSE,
  filterRAE = NULL,
  ...
) {

  #prepare table
  if (prep_tbl) {
    suppressWarnings(
    data_df <- prep_table(df, rm.start, preprocess, transp, add_t, time_res)
    )
    } else {
    data_df <- df
    }
  # analyse period and store result in list
  unique_ids = unique(data_df$ID)
  results_list <- list()
  traces_list <- list()
  fit_trace_list <- list()
  smooth_trace_list <- list()
  plot_list <- list()

  message("                |--- Computing period...")
  cli::cli_progress_bar("Processing", total = length(unique_ids))
  #pb <- txtProgressBar(min = 2, max = length(unique_ids), style = 3)
  for (i in seq_len(length(unique_ids))) {
    # Filter data for the current ID
    ID = unique_ids[i]
    toget = which(data_df$ID == ID)
    data_ID <- data_df[toget, ]

    # perform linear detrending
    fit <- lm(data_ID$value ~ data_ID$t)
    detrended_data <- residuals(fit)

    # if required, invert the data
    if (invert == TRUE) {
      detrended_data = detrended_data * -1
    }

    if (length(detrended_data == length(data_ID$values))) {
      data_ID$value <- detrended_data
    }

    # smooth data with loess
    smooth_param <- loess(data_ID$value ~ data_ID$t, span = 0.08)
    data_ID$smooth <- smooth_param$fitted

    # Estimate period and other parameters
    result <- FFT_NLLS_analyse(
      data_ID,
      minPer = bottom,
      maxPer = top,
      smooth = smooth,
      invert = invert
    )

    # Add the ID to the result
    result$ID <- as.integer(ID)

    # add the detrended trace to a list for export
    traces_list[[ID]] <- detrended_data
    fit_trace_list[[ID]] <- result$fitted_trace
    smooth_trace_list[[ID]] <- data_ID$smooth

    # subset results list to omit fitted trace
    result$fitted_trace <- NULL

    # Append the result to the results list
    results_list[[ID]] <- result

    # setTxtProgressBar(pb, i)
    # cli::cli_inform("Processing ID {i}")
    cli::cli_progress_update()
  }

  # close(pb)
  cli::cli_progress_done()
  message("                |--- Completed")
  # merge all period data and traces into tables

  period_table = do.call(rbind, lapply(results_list, as.data.frame))
  traces_table = do.call(cbind, lapply(traces_list, as.data.frame)) %>%
    t() %>%
    `rownames<-`(names(traces_list))
  fit_table = do.call(cbind, lapply(fit_trace_list, as.data.frame)) %>%
    t() %>%
    `rownames<-`(names(fit_trace_list))
  smooth_table = do.call(cbind, lapply(smooth_trace_list, as.data.frame)) %>%
    t() %>%
    `rownames<-`(names(smooth_trace_list))
  colnames(fit_table) = colnames(traces_table)
  colnames(smooth_table) = colnames(traces_table)


  # count total traces
  period_counts <- sum(!is.na(period_table$phase_h))
  # clean table of all values outside of limits and count valid traces
  period_tbl_allvals <- period_table
  if (excludeNC == TRUE) {
    topvals <- which(period_table$period >= top)
    bottomvals <- which(period_table$period <= bottom)
    exclude = c(topvals, bottomvals)
    period_table[exclude, ] <- NA
    period_tbl_allvals$keep[exclude] <- 0

    #print(paste("Values >", top, "h | <", bottom, "h removed", sep = ""))
  }
  if (!is.null(filterRAE)) {
    badvals <- which(period_table$RAE >= filterRAE)
    period_table[badvals, ] <- NA
    period_tbl_allvals$keep[badvals] <- 0

    #print(paste("RAE vals exceeding ", filterRAE, " removed", sep = ""))
  }
  non_na_count <- sum(!is.na(period_table$phase_h))

  # clean table of NA values
  period_table <- period_table[complete.cases(period_table),]

  # add a column for normalized phase values
  period_table$phase_norm <- normalize_phase(period_table$phase_rad)
  # round all digits in the table
  period_table <- period_table %>%
    dplyr::mutate(dplyr::across(.cols = -all_of("ID"), ~ round(., 2)))

  # list to return results and traces of period analysis
  return_list = list(
    traces = traces_table,
    fit_traces = fit_table,
    smooth_traces = smooth_table,
    period_table = period_table,
    period_table_unfiltered = period_tbl_allvals
  )

  return(return_list)
}

# function to generate a coherence analysis
#' Compute local spatial coherence for a period-analysis variable
#'
#' @param period_table Data frame of period-analysis results; must contain an
#'   \code{ID} column and the column named by \code{variable}.
#' @param grid_coord Data frame of grid coordinates with three columns: ID,
#'   X, Y.
#' @param variable Character; name of the column in \code{period_table} to
#'   use as the coherence metric.
#' @param radius Numeric; neighbourhood radius in micrometres. Defaults to
#'   \code{100}.
#' @param threshold Numeric; minimum coherence value to retain a cell.
#'   Defaults to \code{0.1}.
#' @param merge Logical; if \code{TRUE}, merges coherence results back into
#'   \code{period_table}. Defaults to \code{TRUE}.
#' @param ... Currently unused.
#'
#' @return A data frame of per-cell coherence scores merged with the input
#'   \code{period_table}.
#' @keywords internal
coherence_analysis <- function(
  period_table,
  grid_coord,
  variable,
  radius = 100,
  threshold = 0.1,
  merge = TRUE,
  ...
) {
  plot_data <- grid_coord %>% `colnames<-`(c("ID", "X", "Y"))
  # Merge tables to include period data
  merged_data <- left_join(plot_data, period_table, by = "ID") %>%
    .[!is.na(.$keep), ]

  # Choose the variable to visualize (e.g., period, amplitude, error)
  variable <- variable

  # list all cells within radius value
  coherence_results <- lapply(seq_len(nrow(merged_data)), function(i) {

    cell_id <- merged_data$ID[i]
    x_coord <- merged_data$X[i]
    y_coord <- merged_data$Y[i]

    distances <- sqrt(
      (merged_data$X - x_coord)^2 +
        (merged_data$Y - y_coord)^2
    )

    var_value <- merged_data[[variable]][i]

    nearby_mask <- distances <= radius & merged_data$ID != cell_id
    # Get periods of nearby cells
    nearby_var <- merged_data[[variable]][nearby_mask]

    if (length(nearby_var) == 0) return(NA) # No nearby cells

    # Calculate the absolute differences in periods
    var_diffs <- abs(nearby_var - var_value)

    # Count how many differences are within the threshold
    coherence_count <- sum(var_diffs <= threshold)

    # calculate the ratio between the coherence count and the number of nearby cells
    if (length(nearby_var) > 0) {
      coherence_ratio <- coherence_count / length(nearby_var)
    } else {
      coherence_ratio <- NA # No nearby cells
    }
    # return a table containing the ID and coherence ratio
    results <- data.frame(ID = cell_id, coherence_ratio = coherence_ratio)

    return(results)
  }
  )

  coherence_results <- do.call(rbind, coherence_results)
  # safety check to avoid returning an incompatible output
  if(!identical(colnames(coherence_results), c("ID", "coherence_ratio"))) {
    coherence_results <- data.frame(ID = NA,
                                    coherence_ratio = NA)
  }
  if (merge) {
    # Merge coherence results with the original period table
    coherence_results <- left_join(coherence_results, period_table, by = "ID")
  }

  return(coherence_results)
}

# perform period analysis
#' Estimate period, phase, and amplitude using FFT initialisation and NLLS fitting
#'
#' @param data Data frame with columns \code{t} (time in hours), \code{value}
#'   (raw fluorescence), and \code{smooth} (smoothed fluorescence).
#' @param minPer Numeric; lower period bound in hours. Defaults to \code{16}.
#' @param maxPer Numeric; upper period bound in hours. Defaults to \code{32}.
#' @param smooth Logical; if \code{TRUE}, fits the smoothed trace; if
#'   \code{FALSE}, fits the raw trace. Defaults to \code{TRUE}.
#' @param invert Logical; if \code{TRUE}, inverts the signal before fitting.
#'   Defaults to \code{FALSE}.
#'
#' @return A named list with fitted \code{period}, \code{phase},
#'   \code{amplitude}, \code{RAE}, and \code{fitted_trace}.
#' @keywords internal
FFT_NLLS_analyse <- function(
  data,
  minPer = 16,
  maxPer = 32,
  smooth = TRUE,
  invert = FALSE
) {
  if (smooth) {
    vals <- data$smooth
  } else {
    vals <- data$value
  }
  # Perform FFT
  n <- length(vals)
  dt <- mean(diff(data$t))
  fft_result <- stats::fft(vals)
  frequencies <- seq(0, 1 / dt, length.out = n)

  # Identify the dominant frequency (excluding the zero frequency)
  dominant_frequency <- frequencies[
    which.max(base::Mod(fft_result)[2:(n / 2)]) + 1
  ]
  initial_period <- 1 / dominant_frequency

  # Define the sinusoidal model function
  sinusoidal_model <- function(params, t) {
    amplitude <- params[1]
    period <- params[2]
    phase <- params[3]
    offset <- params[4]
    return(amplitude * sin(2 * pi * t / period + phase) + offset)
  }

  # Define the residual function for nonlinear least squares
  residuals <- function(params, t, y) {
    return(y - sinusoidal_model(params, t))
  }

  # Initial parameter estimates
  initial_amplitude <- (max(vals) - min(vals)) / 2
  initial_phase <- 0
  initial_offset <- mean(vals)
  initial_params <- c(
    initial_amplitude,
    initial_period,
    initial_phase,
    initial_offset
  )

  # Perform nonlinear least squares fitting
  suppressWarnings(
  fit <- minpack.lm::nls.lm(
    par = initial_params,
    fn = residuals,
    t = data$t,
    y = vals,
    lower = c(-Inf, minPer, -Inf, -Inf),
    upper = c(Inf, maxPer, Inf, Inf)
  )
  )

  # Extract fitted parameters
  fitted_params <- fit$par
  fitted_amplitude <- fitted_params[1]
  fitted_period <- fitted_params[2]
  fitted_phase <- fitted_params[3]
  fitted_offset <- fitted_params[4]

  # Ensure the amplitude is positive
  if (fitted_amplitude < 0) {
    fitted_amplitude <- abs(fitted_amplitude)
    fitted_params[1] <- fitted_amplitude
    fitted_phase <- fitted_phase + pi # Adjust the phase by 180 degrees (π radians)
    fitted_params[3] <- fitted_phase
  }

  # save fitted phase before manipulation
  fitted_phase_original <- fitted_phase

  if (invert) {
    fitted_phase <- fitted_phase #+ pi
  }

  # Wrap the phase into the 0-2pi radians range and mirror it
  fitted_phase <- (2 * pi - fitted_phase) %% (2 * pi)
  if (fitted_phase < 0) {
    fitted_phase <- (2 * pi - fitted_phase) + 2 * pi
  }

  # Convert phase to time units (hours)
  fitted_phase <- circular::as.circular(
    fitted_phase,
    type = "angles",
    units = "radians",
    rotation = "clock",
    template = "none",
    modulo = "asis",
    zero = 0
  )
  phase_circadian <- circular::conversion.circular(
    fitted_phase,
    units = "hours"
  )
  phase_absolute <- phase_circadian * (fitted_period / 24)

  # Peak-based Phase Calculation #

  # Find peaks using the raw data
  peaks <- pracma::findpeaks(vals, nups = 5, ndowns = 5)

  if (!is.null(peaks)) {
    peak_times <- data$t[peaks[, 2]] # Get peak times from peak indices

    # First peak
    first_peak_time <- min(peak_times)
    first_peak_phase <- (first_peak_time %% fitted_period) / fitted_period * 24 # Convert to circadian hours

    # Last peak
    last_peak_time <- max(peak_times)
    last_peak_phase <- (last_peak_time %% fitted_period) / fitted_period * 24

    # Mean phase
    mean_peak_phase <- mean((peak_times %% fitted_period) / fitted_period * 24)
  } else {
    first_peak_phase <- NA
    last_peak_phase <- NA
    mean_peak_phase <- NA
  }

  # Compute residual error
  fitted_values <- sinusoidal_model(fitted_params, data$t)
  residual_error <- sqrt(mean((vals - fitted_values)^2))

  # Calculate R-squared (GOF)
  ss_total <- sum((vals - mean(vals))^2)
  ss_res <- sum((vals - fitted_values)^2)
  r_squared <- 1 - (ss_res / ss_total)

  # Compute standard deviation of residuals
  residual_std <- sqrt(
    sum((vals - fitted_values)^2) / (length(vals) - length(fitted_params))
  )

  # Compute RAE
  RAE <- residual_std / fitted_amplitude

  # Return the results
  result <- list(
    keep = TRUE,
    period = fitted_period,
    amplitude = fitted_amplitude,
    FFT_phase = fitted_phase_original,
    phase_h = phase_absolute,
    phase_rad = fitted_phase,
    phase_circ = phase_circadian,
    first_peak_phase_h = first_peak_phase,
    last_peak_phase_h = last_peak_phase,
    mean_peak_phase_h = mean_peak_phase,
    offset = fitted_offset,
    error = residual_error,
    GOF = r_squared,
    RAE = RAE,
    fitted_trace = fitted_values
  )
  return(result)
}

#' Run period analysis on the mean fluorescence trace of a channel
#'
#' @param channel_ctx List; channel context object containing elements
#'   \code{grid_vals} (numeric matrix, cells × frames) and \code{invert}
#'   (logical).
#' @param filename Character; file identifier passed to \code{computePeriod()}.
#' @param time_res Numeric; temporal resolution in hours per frame.
#'
#' @return A one-row data frame of period-analysis results for the mean trace.
#' @keywords internal
mean_trace_period <- function(channel_ctx, filename, time_res) {

  mean_trace <- colMeans(channel_ctx$grid_vals, na.rm = TRUE)
  matrix_onetrace <- matrix(rep(mean_trace, each = 2), nrow = 2)
  colnames(matrix_onetrace) <- colnames(channel_ctx$grid_vals)
  rownames(matrix_onetrace) <- seq(1, nrow(matrix_onetrace))
  suppressMessages(
    period_mean_res <- computePeriod(
      matrix_onetrace,
      filename,
      excludeNC = TRUE,
      top = 36,
      bottom = 16,
      time_res = time_res,
      invert = channel_ctx$invert,
      filterRAE = 0.90)
  )
  results_mean_trace <- period_mean_res$period_table[1, ]

  return(results_mean_trace)
}

#' Compute mutual information between two normalised variables
#'
#' @param var1 Numeric vector of values in \code{[0, 1]}.
#' @param var2 Numeric vector of values in \code{[0, 1]}, same length as
#'   \code{var1}.
#' @param n_bins Integer; number of bins for discretisation. Defaults to
#'   \code{10}.
#' @param normalize Logical; if \code{TRUE}, normalises MI by the joint
#'   entropy. Defaults to \code{FALSE}.
#'
#' @return Numeric; the mutual information value, or \code{NA} if any input
#'   values are \code{NA}.
#' @keywords internal
mutual_information <- function(var1, var2, n_bins = 10, normalize = FALSE) {

  x = var1
  y = var2
  # Check if inputs are valid
  if (length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if (any(x < 0 | x > 1, na.rm = TRUE) || any(y < 0 | y > 1, na.rm = TRUE)) {
    stop("Inputs must be normalized to [0,1]")
  }
  if (any(is.na(x) | is.na(y))) return(mi = NA)

  # Discretize
  x_bin <- cut(
    x,
    breaks = seq(0, 1, length.out = n_bins + 1),
    include.lowest = TRUE,
    labels = FALSE
  )
  y_bin <- cut(
    y,
    breaks = seq(0, 1, length.out = n_bins + 1),
    include.lowest = TRUE,
    labels = FALSE
  )

  # Build joint frequency table
  joint_table <- table(x_bin, y_bin)
  joint_probs <- joint_table / sum(joint_table)

  # Marginal probabilities
  px <- rowSums(joint_probs)
  py <- colSums(joint_probs)

  # Compute mutual information
  mi <- 0
  for (i in seq_along(px)) {
    for (j in seq_along(py)) {
      pxy <- joint_probs[i, j]
      if (pxy > 0 && px[i] > 0 && py[j] > 0) {
        mi <- mi + pxy * log2(pxy / (px[i] * py[j]))
      }
    }
  }

  # Optional normalization: normalized MI in [0, 1]
  if (normalize) {
    h_x <- -sum(px[px > 0] * log2(px[px > 0]))
    h_y <- -sum(py[py > 0] * log2(py[py > 0]))
    mi <- mi / min(h_x, h_y) # or (h_x + h_y)/2 for symmetric NMI
  }

  return(mi)
}

#' Build and cluster a cell correlation network
#'
#' @param filename Character; file identifier used in plot filenames.
#' @param interval_name Character; interval label used in plot filenames.
#' @param cell_traces Numeric matrix of fluorescence traces (cells × frames);
#'   row names must be cell IDs.
#' @param grid_coord Data frame of grid coordinates with columns \code{ID},
#'   \code{X}, and \code{Y}.
#' @param period_table Data frame of period-analysis results containing an
#'   \code{ID} column.
#' @param network_dir Character; directory for saving network output files.
#' @param ... Optional: \code{plot_matrix} (logical, save correlation
#'   heatmap); \code{filter_nonrhythmic} (logical, remove NA-weight edges).
#'
#' @return A named list with elements \code{igraph_obj}, \code{node_data},
#'   and \code{network_vars}.
#'
#' @export
network_analysis <- function(filename,
                             interval_name,
                             cell_traces,
                             grid_coord,
                             period_table,
                             network_dir,
                             ...
) {

  args <- list(...)

  plot_matrix <- args$plot_matrix
  plot_matrix <- ifelse(is.null(plot_matrix), FALSE, plot_matrix)

  filter_nonrhythmic <- args$filter_nonrhythmic
  filter_nonrhythmic <- ifelse(is.null(filter_nonrhythmic),
                               TRUE,
                               filter_nonrhythmic)

  # remove rows containing NA values
  period_table <- period_table[complete.cases(period_table), ]

  cell_names <-data.frame("del" = 0 ,"ID" = as.integer(rownames(cell_traces)))
  colnames(grid_coord) <- c("ID", "X", "Y")
  rownames(grid_coord) <- grid_coord$ID

  cell_coords <- left_join(cell_names, grid_coord, by = "ID") %>% select(-del)
  cell_nodes <- right_join(period_table, cell_coords, by = "ID")

  # # Now merge with period_table
  cell_nodes <- cell_nodes[, c("ID", setdiff(names(cell_nodes), "ID"))]
  names(cell_nodes)[1] <- "name"
  cell_nodes$name <- as.numeric(cell_nodes$name)

  # Reorder cell_nodes to match cell_traces row order:
  # TODO add the matching as a test
  trace_ids <- as.integer(rownames(cell_traces))
  cell_nodes <- cell_nodes[match(trace_ids, cell_nodes$name), ]

  # correlate traces
  edges_corr <- traces_correlation(method = "combined",
                                   traces = cell_traces,
                                   filename = filename,
                                   interval_name = interval_name,
                                   network_dir = network_dir,
                                   grid_coord = grid_coord,
                                   cell_nodes = cell_nodes,
                                   filter_nonrhythmic = TRUE,
                                   plot_matrix = plot_matrix
  )

  # create igraph
  g <- igraph::graph_from_data_frame(d = edges_corr,
                                     vertices = cell_nodes,
                                     directed = FALSE)

  # Calculate node metrics
  node_metrics_tbl <- node_metrics(g)
  node_data <- left_join(cell_nodes, node_metrics_tbl, "name")

  # Local clustering coefficient for each node
  clust_coef <- transitivity(g, type = "localundirected", isolates = "zero")
  # TODO create table with cluster summary coefficients

  # Global clustering coefficient
  global_clust <- transitivity(g, type = "global")

  # Community detection using Louvain method (good for weighted graphs)
  # TODO add additional methods for community detection (i.e. Kmeans)
  comm <- cluster_leiden(g,
                         weights = E(g)$weight,
                         n_iterations = 200,
                         resolution = 0.75,
                         objective_function = "modularity"
  )


  # Membership vector (which cluster each node belongs to)
  membership_vec <- membership(comm)

  # Modularity score (how strong the community structure is)
  modularity_score <- modularity(g, membership_vec, weights = E(g)$weight)

  # add back to the node data
  node_data$cluster <- as.integer(membership_vec)
  node_data$clust_coef <- clust_coef

  # returns g, node_data and mean_phase_per_cluster
  ordered_clusters <- order_clusters(graph_obj = g,
                                     node_data = node_data,
                                     cluster_order = "phase",
                                     network_dir = network_dir)

  g <- ordered_clusters$g

  node_data <- ordered_clusters$node_data

  color_map <- assign_cluster_colors(node_data) # TODO find function

  # create 3D plot representing amplitude, phase coherence (stored in node_data)
  # and cluster size (stored in cluster_size)

  cluster_summary_original <- summarise_cluster_data(node_data)

  cluster_summary_original <- left_join(cluster_summary_original,
                                        color_map,
                                        by = c("cluster")
  )

  segregation_ind <- compute_spatial_segregation(node_data)

  # Filter edges based on module
  g_filtered <- filter_edges(g)

  # Prepare network layout
  layout_list <- network_plot_layout(g_filtered, cell_nodes, color_map)

  # calculate cluster statistics
  cluster_stats <- cluster_stats(node_data)

  # correlate mean cluster phase and mean cluster edges strength
  summary_nodes <- node_data %>%
    group_by(cluster) %>%
    summarise(
      size = n(),
      mean_phase = as.numeric(mean(phase_norm, na.rm = TRUE)),
      mean_strength = mean(strength, na.rm = TRUE),
      mean_degree = mean(degree, na.rm = TRUE),
      mean_clust_coef = mean(clust_coef, na.rm = TRUE)
    )

  network_vars_list = list(global_clust = global_clust,
                           modularity_score = modularity_score,
                           node_metrics = node_metrics_tbl,
                           cluster_df = cluster_summary_original,
                           summary_nodes = summary_nodes,
                           mean_cluster_phases = cluster_stats$mean_cluster_phases,
                           variance_cluster_phases = cluster_stats$variance_cluster_phases,
                           cluster_phases_variance = cluster_stats$cluster_phases_variance,
                           segregation_idx = segregation_ind
  )


  network_return_list <- list(node_data = node_data,
                              igraph = g,
                              network_vars = network_vars_list
  )
  return(network_return_list)
}

#' Normalise phases to hours centred around zero
#'
#' @param angles Numeric vector or circular object of phase values in radians.
#'
#' @return Numeric vector of phase values in hours, centred around zero in
#'   the range \code{[-12, 12]}.
#' @keywords internal
normalize_phase <- function(angles) {

  if(!(length(angles)>0)) return(angles_in_hours <- numeric(0))

  # Ensure input is a circular object
  angles_circ <- circular(
    angles,
    type = "angles",
    units = "radians",
    modulo = "asis"
  )

  # Compute circular mean
  mean_angle <- mean.circular(angles_circ, na.rm = TRUE)

  # Normalize angles to be centered around 0 in the [-pi, pi] range
  normalized_angles <- (angles - as.numeric(mean_angle) + pi) %% (2 * pi) - pi

  # Convert radians to hours (scale π to 12)
  angles_in_hours <- normalized_angles * (12 / pi)

  return(angles_in_hours)
}

# prepare data into long table format
#' Reshape and optionally preprocess a trace matrix into long format
#'
#' @param data Numeric matrix of fluorescence traces (cells × frames), with
#'   an optional leading ID column.
#' @param rm.start Integer; number of initial time points to remove. Defaults
#'   to \code{0}.
#' @param preprocess Logical; if \code{TRUE}, calls \code{preprocess_data()}
#'   to smooth and detrend. Defaults to \code{TRUE}.
#' @param transp Logical; if \code{TRUE}, transposes \code{data} before
#'   processing. Defaults to \code{TRUE}.
#' @param add_t Logical; if \code{TRUE}, adds a time column based on
#'   \code{time_res}. Defaults to \code{TRUE}.
#' @param time_res Numeric; temporal resolution in hours per frame.
#'
#' @return A long-format \code{data.table} with columns \code{ID}, \code{t},
#'   \code{value}, and \code{smooth}.
#' @keywords internal
prep_table <- function(
  data,
  rm.start = 0,
  preprocess = TRUE,
  transp = TRUE,
  add_t = TRUE,
  time_res
) {
  # re-transpose table
  if (transp) {
    data = t(data)
  }

  # remove initial data
  if (rm.start != 0) {
    print("removing data")
    data = data[-c(seq(1, rm.start)), ]
  }

  # add time column
  if (add_t) {
    t = seq(0, length.out = length(data[, 1]), by = time_res)
    data = cbind(t, data)
  }

  # perform outliers removal, smoothening and detrending
  if (preprocess) {
    data_clean <- preprocess_data(data, grade = 3)
  } else {
    data_clean <- as.data.frame(data)
  }

  #make it into long table format
  data_long <- tidyr::pivot_longer(
    data_clean,
    cols = colnames(data_clean)[-1],
    names_to = "ID"
  ) %>%
    data.table::as.data.table(key = "ID")

  return(data_long)
}

# function to remove outliers, smooth and detrend
#' Remove outliers, smooth, and detrend fluorescence traces
#'
#' @param data Numeric matrix of fluorescence traces with a leading time
#'   column (time × cells).
#' @param grade Integer; polynomial degree used for detrending.
#' @param mode Character; smoothing method — currently only
#'   \code{"mov_avg"} (moving average) is supported. Defaults to
#'   \code{"mov_avg"}.
#' @param parallel Logical; reserved for future parallelisation. Defaults to
#'   \code{FALSE}.
#'
#' @return A numeric matrix of the same dimensions as \code{data} with
#'   smoothed and detrended traces.
#' @export
preprocess_data <- function(data, grade, mode = "mov_avg", parallel = FALSE) {
  # load table
  data_to_clear <- data

  # create table to store smoothened data
  n <- nrow(data)
  p <- ncol(data)

  clear_mat <- matrix(NA_real_, n, p)
  clear_mat[,1] <- data[,1]
  # get time values
  x_vals <- data[, 1]
  t_index <- seq_len(nrow(data))
  #message("              |--- Preprocessing data...")
  # pb <- txtProgressBar(
  #   min = 2,
  #   max = length(colnames(data_to_clear)),
  #   style = 3
  # )
  pad <- 2

  # for cycle to go through each column and smooth the data
# TODO implement parallelisation
  for (i in 2:p) {

    timeserie <- data_to_clear[, i]

    # create vector for cleaned data
    cleaned_ts <- timeserie

  switch (mode,
    "mov_avg" = {

      trend <- stats::lm(timeserie ~ poly(t_index, grade))
      detrended <- resid(trend)

      # outliers detection and removal
      sigma <- mad(detrended, constant = 1)
      outliers <- abs(detrended) > 2 * sigma

      # delete outliers from data
      detrended[outliers] <- NA
      # interpolate missing values
      interpolated_ts <- imputeTS::na_interpolation(detrended, option = "spline")

      # # perform smoothening of the data
      # fit <- smooth.spline(t_index, interpolated_ts, spar = 0.6)
      # smoothed_values <- predict(fit, t_index)$y

      # add edges padding
      x_pad <- c(
        interpolated_ts[1:pad],
        interpolated_ts,
        interpolated_ts[(length(interpolated_ts) - pad + 1):length(interpolated_ts)]
      )
      # moving average method
      averaged_pad <- stats::filter(
        x_pad,
        rep(1 / 5, 5),
        sides = 2
      )
      # remove edges
      averaged_ts <- averaged_pad[(pad + 1):(length(averaged_pad) - pad)]

      # add smoothened data to matrix
      clear_mat[, i] <- averaged_ts

    },
    "loess" = {



        # perform first loess approximation
        smoothed_series <- loess(timeserie ~ t_index, span = 0.08)
        smoothed_values <- predict(
          smoothed_series,
          newdata = data.frame(x = x_vals)
        )

        # calculate outliers based on distance from loess curve and sd
        stdev_ts <- sd(smoothed_values)
        outliers <- which(abs(smoothed_values - timeserie) > stdev_ts * 0.6)

        # delete outliers from data
        cleaned_ts[outliers] <- NA

        # perform second loess approximation on clean data
        smoothed_clean_series <- loess(
          cleaned_ts ~ seq_along(cleaned_ts),
          span = 0.08
        )
        smoothed_clean_values <- predict(
          smoothed_clean_series,
          newdata = data.frame(x = x_vals)
        )

        # interpolate missing values before detrending
        smoothed_clean_values <- imputeTS::na_interpolation(
          smoothed_clean_values,
          option = "spline"
        )

        # add detrending step
        smoothed_clean_detr_values <- astsa::detrend(smoothed_clean_values, grade)

        # add smoothened data to matrix
        clear_mat[, i] <- smoothed_clean_detr_values

        # update progressbar
        #setTxtProgressBar(pb, i)
      }
  )
  }

  #close(pb)
  #message("              |--- Completed")
  clear_data <- as.data.frame(clear_mat)
  # re add colnames
  colnames(clear_data) <- colnames(data)
  # return new table
  return(clear_data)
}


# function to align traces to the mean phase of the group
#' Circularly shift traces so their phases align to the group mean
#'
#' @param traces_table Numeric matrix of fluorescence traces (cells × frames).
#' @param period_table Data frame of period-analysis results with a
#'   \code{phase_rad} column.
#' @param remove_start Integer; frames to discard from the start after
#'   shifting. Defaults to \code{0}.
#' @param remove_end Integer; frames to discard from the end after shifting.
#'   Defaults to \code{0}.
#' @param align_to Numeric; target phase in hours; if \code{NA}, uses the
#'   circular mean of the group. Defaults to \code{NA}.
#' @param debug Logical; if \code{TRUE}, enables debugging output. Defaults
#'   to \code{FALSE}.
#'
#' @return A numeric matrix of phase-aligned traces, same dimensions as
#'   \code{traces_table} (minus any removed frames).
#' @keywords internal
phase_align_trace <- function(
  traces_table,
  period_table,
  remove_start = 0,
  remove_end = 0,
  align_to = NA,
  debug = FALSE
) {
  # TODO check align phase value

  # transform rad values in plus minus Pi
  phases = circular::minusPiPlusPi(circular(
    period_tbl$phase_rad,
    units = "rad"
  ))
  if (is.na(align_to)) {
    align_phase = circular::mean.circular(phases, na.rm = TRUE)
  } else {
    align_phase = circular::as.circular(
      (align_to / 12) * pi,
      type = "angles",
      units = "radians",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    )
  }
  ph_diff = align_phase - phases
  adjust = circular::minusPiPlusPi(circular(ph_diff, units = "rad"))
  adjust_frames = round(
    circular::conversion.circular(adjust, units = "hours") * 2,
    0
  )
  # initialize new table to store vecs
  traces_aligned <- replace(traces_table, all(), NA)
  for (i in 1:dim(traces_table)[1]) {
    # get vector
    trace = traces_table[i, ]
    adjust_fct = adjust_frames[i]
    if (is.na(adjust_fct)) {
      clean_trace = rep(NA, dim(traces_table)[2])
    } else if (adjust_fct == 0) {
      clean_trace = trace
    } else if (adjust_fct < 0) {
      clean_trace = c(
        rep_len(NA, abs(adjust_fct)),
        trace[1:(length(trace) - abs(adjust_fct))]
      )
    } else if (adjust_fct > 0) {
      clean_trace = c(
        trace[(abs(adjust_fct) + 1):length(trace)],
        rep_len(NA, abs(adjust_fct))
      )
    }
    traces_aligned[i, ] = clean_trace
    if (debug) {
      # browser()
      plot(x = 0:(dim(traces_table)[2] - 1), y = trace, col = "red", type = "l")
      lines(
        x = 0:(dim(traces_table)[2] - 1),
        y = clean_trace,
        col = "blue",
        type = "l"
      )
    }
  }
  # trim start and end of table
  # evaluate if all have been shifted in one direction
  # min_shift = min(adjust_frames, na.rm = TRUE)
  # max_shift = max(adjust_frames, na.rm = TRUE)
  # shift = min_shift*max_shift
  # if(shift > 0){
  #   # case when all the shift happens in the same direction (need to cut only from one of the two ends)
  #   # TODO complete trimming in another function
  # } else if(shift < 0){
  #   # case when shift happens in both direction (you can use the min-max rune simply)
  #   traces_aligned_trim = traces_aligned[ , (max(adjust_frames, na.rm = TRUE)+1):(dim(traces_aligned)[2]+min(adjust_frames, na.rm = TRUE))]
  # }

  #traces_aligned_trim = traces_aligned[ , (max(adjust_frames, na.rm = TRUE)+1):(dim(traces_aligned)[2]+min(adjust_frames, na.rm = TRUE))]

  return(traces_aligned)
}
ranges_copy <- function(params) {
  # TODO remove if and make function proper
  if(!is.null(params$plotting$ranges)){
    message("\nLoading plot ranges from previous experiment")
    old_params_path <- file.path(previous_exp, "summary_stats", "plot_limits.rds")
    old_params <- readRDS(old_params_path)

    params$plotting$ranges <- old_params$plotting$ranges
    '
    # assign all period ranges from previous experiment into params
    if(Channel2_an){
      period_range_ch2 <- plot_ranges$period_range_ch2
      amplitude_range_ch2 <- plot_ranges$amplitude_range_ch2
      RAE_range_ch2 <- plot_ranges$RAE_range_ch2
      AUC_range_ch2 <- plot_ranges$AUC_range_ch2
      phase_y_lims_Ch2 <- plot_ranges$phase_y_lims_Ch2
    }
    if(Channel1_an & Channel2_an){
      phase_y_lims_Chx <- plot_ranges$phase_y_lims_Chx
    }
    '
    Y_range_loaded = TRUE
  }
}

#' Compute per-variable display ranges across all files and intervals
#'
#' @param params List of analysis parameters produced by \code{make_params()};
#'   must contain \code{channels} and \code{time$intervals}.
#' @param file_rows Data frame of file metadata; must contain columns
#'   \code{file_id}, \code{file_path}, and \code{folder_path}.
#'
#' @return A named list (by channel) of per-variable min/max ranges used
#'   to set consistent colour scales across plots.
#' @export
ranges_calculation <- function(params, file_rows) {

channel_ranges <- purrr::map(names(params$channels), function(ch_id) {
    if (!params$channels[[ch_id]]$enabled) return(NULL)
    # get all period tables
    # TODO create a save function for parameters and retrieve params from path

  # TODO make these two into functions
  period_tbl_list <- unlist(
    purrr::pmap(
      file_rows,
      function(file_id, file_path, folder_path,...) {

        file_row <- list(file_id = file_id,
                         file_path = file_path,
                         folder_path = folder_path)

        # Create a list for each interval
        interval_tables <- setNames(
          lapply(names(params$time$intervals), function(int_name) {
            period_tbl_path <- file.path(file_row$folder_path, "rds",
                                         paste0(file_row$file_id, "_",
                                                int_name, "_",
                                                ch_id, "_period_tbl_clean.rds"))

            if(file.exists(period_tbl_path)) {
              readRDS(period_tbl_path)
            } else {
              warning("File not found: ", period_tbl_path)
              NULL
            }
          }),
          paste0(file_id, "_", names(params$time$intervals))
        )

        return(interval_tables)
      }),
    recursive = FALSE
  )

  # Apply the same pattern to auc_tbl_list:
  auc_tbl_list <- unlist(
    purrr::pmap(
      file_rows,
      function(file_id, file_path, folder_path,...) {

        file_row <- list(file_id = file_id,
                         file_path = file_path,
                         folder_path = folder_path)

        interval_tables <- setNames(
          lapply(names(params$time$intervals), function(int_name) {
            auc_tbl_path <- file.path(file_row$folder_path, "rds",
                                      paste0(file_row$file_id, "_",
                                             int_name, "_",
                                             ch_id, "_auc_results.rds"))

            if(file.exists(auc_tbl_path)) {
              readRDS(auc_tbl_path)
            } else {
              warning("File not found: ", auc_tbl_path)
              NULL
            }
          }),
          paste0(file_id, "_", names(params$time$intervals))
        )

        return(interval_tables)
      }),
    recursive = FALSE
  )
# period_tbl_list <- setNames( # TODO make these two into functions
#   purrr::pmap(
#     file_rows,
#     function(file_id, file_path, folder_path, ...) {
#
#       file_row <- list(file_id = file_id,
#                        file_path = file_path,
#                        folder_path = folder_path)
#
#       # generate period_tbl path
#       # TODO fix case with multiple intervals, where it should gather period
#       # tables from all intervals analyzed
#       browser()
#       period_tbl_path <- file.path(file_row$folder_path, "rds",
#                                    paste0(file_row$file_id, "_",
#                                          names(params$time$intervals), "_",
#                                          ch_id, "_period_tbl_clean.rds"
#                                    ))
#       period_tbl <- readRDS(period_tbl_path)
#
#       return(period_tbl)
#     }),
#   file_rows$file_id)
#
# auc_tbl_list <- setNames(
#   purrr::pmap(
#     file_rows,
#     function(file_id, file_path, folder_path, ...) {
#
#       file_row <- list(file_id = file_id,
#                        file_path = file_path,
#                        folder_path = folder_path)
#
#       # generate auc_tbl path
#       # TODO fix case with multiple intervals, where it should gather period
#       # tables from all intervals analyzed
#       auc_tbl_path <- file.path(file_row$folder_path, "rds",
#                                    paste0(file_row$file_id, "_",
#                                           names(params$time$intervals), "_",
#                                           ch_id, "_auc_results.rds"
#                                    ))
#       auc_tbl <- readRDS(auc_tbl_path)
#
#       return(auc_tbl)
#     }),
#   file_rows$file_id)
    '
        period_tbl_allpaths_Ch1 = readRDS(
        file.path(wd, "summary_stats",
        "period_tbl_allpaths_Ch1.rds"))
    Y_range_loaded = FALSE

    # load all period tables into a list

    period_tbl_list_Ch1 <- setNames(lapply(seq_along(period_tbl_allpaths_Ch1),
    function(i) {
      readRDS(period_tbl_allpaths_Ch1[i])
    }),
    filenames)
    '

    # In each object of the list period_tbl_list, calculate the min, max
    # and sd for the required variables

    period_ranges <- lapply(period_tbl_list, function(tbl) {
      if(!nrow(tbl) > 0) return(NULL)
      data.frame(
        period_min = min(tbl$period, na.rm = TRUE),
        period_max = max(tbl$period, na.rm = TRUE),
        period_median = median(tbl$period, na.rm = TRUE),
        period_sd = safe_sd(tbl$period, na.rm = TRUE),
        phase_distr_max = density(tbl$phase_norm, bw = 5) |>
          (\(d) max(d$y))(),
        amplitude_min = min(tbl$amplitude, na.rm = TRUE),
        amplitude_max = max(tbl$amplitude, na.rm = TRUE),
        amplitude_median = median(tbl$amplitude, na.rm = TRUE),
        amplitude_sd = safe_sd(tbl$amplitude, na.rm = TRUE),
        error_min = min(tbl$error, na.rm = TRUE),
        error_max = max(tbl$error, na.rm = TRUE),
        error_median = median(tbl$error, na.rm = TRUE),
        error_sd = safe_sd(tbl$error, na.rm = TRUE),
        RAE_min = min(tbl$RAE, na.rm = TRUE),
        RAE_max = max(tbl$RAE, na.rm = TRUE),
        RAE_median = median(tbl$RAE, na.rm = TRUE),
        RAE_sd = safe_sd(tbl$RAE, na.rm = TRUE)
      )
    })

  # collate all into a table
  period_tbl_ranges_df <- do.call(rbind, period_ranges)

  # if AUC gets computed, get all AUC results
  AUC_ranges <- lapply(auc_tbl_list, function(vec) {

    if(!nrow(vec) > 0) return(NULL)
    data.frame(
      AUC_min = min(vec[,2], na.rm = TRUE),
      AUC_max = max(vec[,2], na.rm = TRUE),
      AUC_median = median(vec[,2], na.rm = TRUE),
      AUC_sd = sd(vec[,2], na.rm = TRUE)
    )
  })

  # collate all into a table
  AUC_tbl_ranges_df <- do.call(rbind, AUC_ranges)

  # determine ranges

  period_range <-
    c(
      round(
        (mean(period_tbl_ranges_df$period_min, na.rm = TRUE)
         - (mean(period_tbl_ranges_df$period_sd, na.rm = TRUE))),
        1),
      round(
        (mean(period_tbl_ranges_df$period_max, na.rm = TRUE)
         + (mean(period_tbl_ranges_df$period_sd, na.rm = TRUE))),
        1)
      )

  amplitude_range <-
    c(
      round(
        (mean(period_tbl_ranges_df$amplitude_min, na.rm = TRUE)
         -(mean(period_tbl_ranges_df$amplitude_sd, na.rm = TRUE)/2)),
        0),
      round(
        (median(period_tbl_ranges_df$amplitude_max, na.rm = TRUE)
         + (mean(period_tbl_ranges_df$amplitude_sd, na.rm = TRUE)/2)),
            0)
      )

  RAE_range <-
    c(
      round(
        (min(period_tbl_ranges_df$RAE_min, na.rm = TRUE)
         -(mean(period_tbl_ranges_df$RAE_sd, na.rm = TRUE)/2)),
        1),
      round(
        (max(period_tbl_ranges_df$RAE_max, na.rm = TRUE)
         + (mean(period_tbl_ranges_df$RAE_sd, na.rm = TRUE)/2)),
        1)
      )

  error_range <-
    c(
      round(
        (min(period_tbl_ranges_df$error_min, na.rm = TRUE)
         -(mean(period_tbl_ranges_df$error_sd, na.rm = TRUE)/2)),
        1),
      round(
        (max(period_tbl_ranges_df$error_max, na.rm = TRUE)
         + (mean(period_tbl_ranges_df$error_sd, na.rm = TRUE)/2)),
        1)
    )

  AUC_range <-
    c(
      round(
        (min(AUC_tbl_ranges_df$AUC_min, na.rm = TRUE)
         - (mean(AUC_tbl_ranges_df$AUC_sd, na.rm = TRUE))),
        0),
      round(
        (median(AUC_tbl_ranges_df$AUC_max, na.rm = TRUE)
         + (mean(AUC_tbl_ranges_df$AUC_sd, na.rm = TRUE))),
        0)
      )

  phase_distr_range <-
    c(0,
      (round(
        max(period_tbl_ranges_df$phase_distr_max, na.rm = TRUE),
        2)*1.3)
    )

  # assign to list to be added to params
  channel_ranges <- list(
    period_range = period_range,
    amplitude_range = amplitude_range,
    phase_distr_range = phase_distr_range,
    RAE_range = RAE_range,
    error_range = error_range,
    AUC_range = AUC_range
  )
  return(channel_ranges)

}) # end of purrr::walk
channel_ranges <- setNames(channel_ranges, names(params$channels))
return(channel_ranges)
}

# create a similarity scores between two tables that share a key variable
#' Compute a pairwise similarity score between two variables
#'
#' @param tbl1 Data frame containing \code{var1} and \code{key}.
#' @param tbl2 Data frame containing \code{var2} and \code{key}.
#' @param var1 Character; column name of the first variable in \code{tbl1}.
#' @param var2 Character; column name of the second variable in \code{tbl2}.
#' @param key Character; name of the shared ID column used to join the
#'   tables. Defaults to \code{"ID"}.
#'
#' @return A data frame with the \code{key} column and a \code{similarity}
#'   column (\code{1 - |var1 - var2|}).
#' @keywords internal
similarity_score <- function(tbl1, tbl2, var1, var2, key = "ID") {
  # browser()
  # Ensure both inputs are data frames
  stopifnot(is.data.frame(tbl1), is.data.frame(tbl2))

  # keyvar = !!sym(key)
  # merge the tables by key
  merged_data <- left_join(tbl1, tbl2, by = key) %>%
    `colnames<-`(c(key, var1, var2))

  # Extract the vectors
  x <- merged_data[[var1]]
  y <- merged_data[[var2]]

  # # Check for values outside 0–1
  # if (any(x < 0 | x > 1, na.rm = TRUE) || any(y < 0 | y > 1, na.rm = TRUE)) {
  #   warning("One or both variables have values outside [0,1].")
  # }
  #
  # # Remove NA pairs
  # valid <- complete.cases(x, y)
  # if (!all(valid)) {
  #   warning(sprintf("Removing %d incomplete rows (NA)", sum(!valid)))
  #   x <- x[valid]
  #   y <- y[valid]
  # }

  # Compute similarity score
  similarity <- 1 - abs(merged_data[[var1]] - merged_data[[var2]])
  merged_data$similarity <- similarity
  #remove var1 and var2 columns
  merged_data <- merged_data %>% select(-!!sym(var1), -!!sym(var2))
  return(merged_data)
}

#' Summarise period analysis results into a single-row table
#'
#' @param period_res Named list as returned by \code{computePeriod()}; must
#'   contain \code{period_table} and \code{period_table_unfiltered}.
#' @param circ_stats Named list as returned by \code{circular_stats()};
#'   must contain \code{vectorLength} and \code{circStats}.
#' @param Ch_rep Character; channel label used as a column prefix. Defaults
#'   to \code{"Ch"}.
#' @param coherence Logical; if \code{TRUE}, includes coherence metrics in
#'   the summary. Defaults to \code{FALSE}.
#'
#' @return A one-row data frame of summary statistics for the analysis.
#' @keywords internal
summarizePeriod <- function(
  period_res,
  circ_stats,
  # filename = "sample",
  # interval_name = "Int",
  Ch_rep = "Ch",
  coherence = FALSE
) {

  period_table <- period_res$period_table
 # TODO add check that
  # trace stats
  period_counts <- length(period_table$phase_h)
  non_na_count <- sum(!is.na(period_res$period_table_unfiltered$phase_h))
  if(non_na_count > 0) {

  # period stats
  period_var = stats::var(period_table$period, na.rm = TRUE) |> round(3)
  period_mean = mean(period_table$period, na.rm = TRUE) |> round(2)
  # phase stats
  circ_phase_mean_rad = circular::mean.circular(
    circular(
      period_table$phase_rad,
      units = "rad",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  ) |> round(3)

  circ_phase_mean = circular::mean.circular(
    circular(
      period_table$phase_circ,
      units = "hour",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  ) |> round(2)

  phase_rad_var = circular::angular.variance(
    circular(
      period_table$phase_rad,
      units = "radians",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  ) |> round(3)

  phase_var = circular::angular.variance(
    circular(
      period_table$phase_circ,
      units = "radians",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  ) |> round(3)

' Deprecated
  # first peak phase
  first_peak_phase_mean = circular::mean.circular(
    circular(
      period_table$first_peak_phase_h,
      units = "hour",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  )

  first_phase_var = circular::angular.variance(
    circular(
      period_table$first_peak_phase_h,
      units = "hour",
      rotation = "clock",
      template = "none",
      modulo = "asis",
      zero = 0
    ),
    na.rm = TRUE
  )
'

  # other stats
  vector_length <- ifelse(!is.null(circ_stats$vectorLength),
                          round(circ_stats$vectorLength, 3),
                          NA)
  amplitude_mean <- mean(abs(period_table$amplitude), na.rm = TRUE) |> round(2)
  amplitude_var <- stats::var(period_table$amplitude, na.rm = TRUE) |> round(3)
  RAE_mean <- mean(period_table$RAE, na.rm = TRUE) |> round(2)
  RAE_var <- stats::var(period_table$RAE, na.rm = TRUE) |> round(2)
  error_mean <- mean(period_table$error, na.rm = TRUE) |> round(2)

  } else {
    period_var = NA
    period_mean = NA
    circ_phase_mean_rad = NA
    phase_rad_var = NA
    vector_length = NA
    circ_phase_mean = NA
    phase_var = NA
    amplitude_mean = NA
    amplitude_var = NA
    RAE_mean = NA
    RAE_var = NA
    error_mean = NA
  }
  # return values outside
  outreturn_df <- data.frame(
    # filename = as.factor(filename),
    # interval = as.factor(interval_name),

    channel_label = as.factor(Ch_rep),                     # channel label
    trace_no = non_na_count,                               # number of rhythmic cells
    trace_tot = period_counts,                             # number of cells sampled
    period_var = period_var,                               # variance of period values
    period_mean = period_mean,                             # mean of period values in hours
    phase_rad_mean = circ_phase_mean_rad,                  # mean of period values in radians
    phase_rad_var = phase_rad_var,                         # circular variance of phases in radians
    vector_length = vector_length,                         # vector length of rayleigh plot
    circ_phase_mean = circ_phase_mean,                     # circular mean of circadian phase values in hours
    circ_phase_var = phase_var,                            # circular variance of circadian phases
    # first_peak_phase_mean = first_peak_phase_mean,
    # first_phase_var = first_phase_var,
    amplitude_mean = amplitude_mean,
    amplitude_var = amplitude_var,
    RAE_mean = RAE_mean,
    RAE_var = RAE_var,
    error_mean = error_mean
  )

  if(coherence){
    # spatial coherence stats

    #period
    period_crn_var = stats::var(period_table$period_crn, na.rm = TRUE) |>
      round(3)
    period_crn_mean = mean(period_table$period_crn, na.rm = TRUE) |>
      round(3)

    #phase
    phase_crn_var = stats::var(period_table$phase_crn, na.rm = TRUE) |>
      round(3)
    phase_crn_mean = mean(period_table$phase_crn, na.rm = TRUE) |>
      round(3)

    #amplitude
    amplitude_crn_var = stats::var(period_table$amp_crn, na.rm = TRUE) |>
      round(3)
    amplitude_crn_mean = mean(period_table$amp_crn, na.rm = TRUE) |>
      round(3)

    # add to table
    outreturn_df <- cbind(outreturn_df,
                          period_crn_mean, period_crn_var,
                          phase_crn_mean, phase_crn_var,
                          amplitude_crn_mean, amplitude_crn_var)
  }

  return(outreturn_df)
}
