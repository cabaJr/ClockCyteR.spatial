# Spatial scatter plot of cluster assignments

Spatial scatter plot of cluster assignments

## Usage

``` r
spatial_clusters_plot(node_data, colors, filename)
```

## Arguments

- node_data:

  Data frame with columns `X`, `Y`, and `cluster`.

- colors:

  Character vector of colors, named by cluster level.

- filename:

  Character; used in the plot title.

## Value

A `ggplot` object.
