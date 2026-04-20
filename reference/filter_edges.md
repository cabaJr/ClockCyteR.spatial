# Filter network edges or vertices belonging to NULL clusters

Filter network edges or vertices belonging to NULL clusters

## Usage

``` r
filter_edges(igraph_obj, delete.edges = TRUE)
```

## Arguments

- igraph_obj:

  An igraph object with vertex attribute `module`.

- delete.edges:

  Logical; if `TRUE`, removes vertices in `"NULL"` clusters entirely; if
  `FALSE`, removes only edges connecting to `"NULL"` vertices. Defaults
  to `TRUE`.

## Value

A filtered igraph object.
