# Reorder and relabel network clusters

Reorder and relabel network clusters

## Usage

``` r
order_clusters(
  graph_obj,
  node_data,
  cluster_order = "size",
  network_dir,
  interval_name = int_list$name,
  cluster_threshold = 3
)
```

## Arguments

- graph_obj:

  An igraph object with vertex attribute `module`.

- node_data:

  Data frame of node metadata including columns `name`, `cluster`, and
  `phase_circ`.

- cluster_order:

  Character; ordering criterion, either `"size"` (decreasing) or
  `"phase"` (ascending mean phase). Defaults to `"size"`.

- network_dir:

  Character; directory used for saving cluster outputs.

- interval_name:

  Character; interval label.

- cluster_threshold:

  Integer; clusters with this many cells or fewer are collapsed into
  `"NULL"`. Defaults to `3`.

## Value

A named list with elements `g` (updated igraph object), `node_data`
(updated data frame with relabelled `cluster` factor), and
`mean_phase_per_cluster` (data frame or `NULL`).
