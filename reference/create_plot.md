# Dispatch a plot by type

Dispatch a plot by type

## Usage

``` r
create_plot(plot_type, plotting_params, ...)
```

## Arguments

- plot_type:

  Character; key identifying which plot to produce (e.g. `"circular"`,
  `"period"`).

- plotting_params:

  List of plotting configuration parameters (ranges, color codes, etc.).

- ...:

  Additional arguments passed to the underlying plot function.

## Value

A named list with elements `plot` (the `ggplot` or base-graphics
object), `plot_name` (character), and `status` (character).
