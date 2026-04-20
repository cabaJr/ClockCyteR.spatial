# Circular stats plot with per-group mean arrows

Circular stats plot with per-group mean arrows

## Usage

``` r
circStats_plot(data, path, colors)
```

## Arguments

- data:

  Named list; each element is a numeric vector of phases in radians for
  one group.

- path:

  Character; file path where the SVG is saved.

- colors:

  Character vector of colors, one per group in `data`.

## Value

Invisibly `NULL`; the plot is written to `path`.
