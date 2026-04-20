# Kernel density plot of phase distribution for one or two channels

Kernel density plot of phase distribution for one or two channels

## Usage

``` r
phase_distribution(
  data,
  color = "#eb1020",
  line_size = 1.2,
  bandwidth = 0.4,
  Ch_rep = "Ch1",
  multi = FALSE,
  data2 = NA,
  color2 = "#159c10",
  Ch2_rep = "Ch2",
  ylims = NA,
  ...
)
```

## Arguments

- data:

  Numeric vector of phases in radians for the primary channel.

- color:

  Character; line color for the primary channel. Defaults to
  `"#eb1020"`.

- line_size:

  Numeric; line width. Defaults to `1.2`.

- bandwidth:

  Numeric; kernel bandwidth passed to `geom_density()`. Defaults to
  `0.4`.

- Ch_rep:

  Character; label for the primary channel in the legend. Defaults to
  `"Ch1"`.

- multi:

  Logical; if `TRUE`, overlays a second density curve using `data2`.
  Defaults to `FALSE`.

- data2:

  Numeric vector of phases in radians for the second channel; used when
  `multi = TRUE`. Defaults to `NA`.

- color2:

  Character; line color for the second channel. Defaults to `"#159c10"`.

- Ch2_rep:

  Character; label for the second channel in the legend. Defaults to
  `"Ch2"`.

- ylims:

  Numeric vector of length 2 for y-axis limits, or `NA` for automatic
  limits. Defaults to `NA`.

- ...:

  Currently unused.

## Value

A `ggplot` object.
