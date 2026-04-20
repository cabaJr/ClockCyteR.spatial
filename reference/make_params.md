# make_params

Constructs and validates the parameter list used throughout the analysis
and plotting pipeline, applying defaults for any unspecified options.

## Usage

``` r
make_params(
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
  plotting = list(y_limits_from_previous = TRUE, previous_exp = "path/to/previous/exp",
    override_ranges = FALSE, ranges = list(), plot_types = c("period", "amplitude",
    "RAE", "AUC", "phase", "phase_norm", "3D"), save_format = "svg", colors = list(Ch1 =
    "darkgreen", Ch2 = "purple", Ch3 = "#e5e372", Ch4 = "#00aefe")),
  folder_structure = list(base_dir = "results", interval_dir = "interval", channel_dir =
    "channel")
)
```

## Arguments

- channels:

  A named list of channel definitions. Each element should contain at
  minimum `enabled` (logical), `label` (character), `invert` (logical),
  and `grid_file` (character).

- coherence:

  Logical; whether to compute spatial coherence metrics. Defaults to
  `TRUE`.

- network:

  Logical; whether to perform network analysis. Defaults to `TRUE`.

- normalize_phase:

  Character string naming the channel to use as phase reference for
  normalisation, or `NULL` to skip. Defaults to `NULL`.

- time_res:

  Numeric; the temporal resolution of the data in minutes per frame.

- intervals:

  A named list of numeric vectors of length 2 defining time intervals
  (start and end in the same units as `time_res`), or `NULL`.

- time_window:

  Logical; whether to subset data to defined intervals. Defaults to
  `FALSE`.

- pixel_fct:

  Numeric; scaling factor converting pixel units to physical units.

- saving:

  Logical; whether to save outputs to disk. Defaults to `TRUE`.

- reload_period:

  Logical; whether to reload previously computed period results from
  cache. Defaults to `FALSE`.

- plotting:

  A named list of plotting options. See Details.

- folder_structure:

  A named list with element `base_dir` specifying the root output
  directory.

## Value

A named list of validated analysis parameters, with defaults applied for
any unspecified plotting options.

## Examples

``` r
if (FALSE) { # \dontrun{
params <- make_params(
  channels = list(Ch1 = list(enabled = TRUE, label = "RCaMP",
                             invert = FALSE, grid_file = "ch1.csv")),
  time_res = 30,
  pixel_fct = 0.8
)
} # }
```
