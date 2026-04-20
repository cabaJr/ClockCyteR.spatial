# Plot the fluorescence trace of a single cell

Plot the fluorescence trace of a single cell

## Usage

``` r
plot_cell(
  cellID,
  data,
  title = "",
  tbl.type = "long",
  xlab = "Time (h)",
  ylab = "Value",
  ...
)
```

## Arguments

- cellID:

  Integer or character; the cell identifier to look up in `data`.

- data:

  Data frame (long format with columns `ID`, `t`, `value`) or numeric
  matrix (wide format, cells × frames).

- title:

  Character; plot title. Defaults to `""`.

- tbl.type:

  Character; `"long"` (default) or `"wide"`.

- xlab:

  Character; x-axis label. Defaults to `"Time (h)"`.

- ylab:

  Character; y-axis label. Defaults to `"Value"`.

- ...:

  Currently unused.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_cell(2245, period_res$traces, tbl.type = "wide")
  plot_cell(2245, channel_ctx$grid_vals, tbl.type = "wide")
} # }
```
