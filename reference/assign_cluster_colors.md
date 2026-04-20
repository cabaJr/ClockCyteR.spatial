# Assign colors to clusters

Assign colors to clusters

## Usage

``` r
assign_cluster_colors(node_data)
```

## Arguments

- node_data:

  Data frame with a `cluster` factor column.

## Value

A data frame with columns `cluster` and `color`. `"NULL"` clusters are
assigned `"grey80"`.
