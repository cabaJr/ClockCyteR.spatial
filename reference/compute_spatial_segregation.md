# Compute a spatial segregation index across clusters

Compute a spatial segregation index across clusters

## Usage

``` r
compute_spatial_segregation(node_data, min_cluster_size = 3)
```

## Arguments

- node_data:

  Data frame of node metadata with columns `cluster`, `X`, and `Y`.

- min_cluster_size:

  Integer; clusters with fewer cells than this are excluded before
  computing the index. Defaults to `3`.

## Value

A named list with elements `global_index` (numeric) and `overlap_matrix`
(square matrix of pairwise cluster overlaps).
