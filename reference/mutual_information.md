# Compute mutual information between two normalised variables

Compute mutual information between two normalised variables

## Usage

``` r
mutual_information(var1, var2, n_bins = 10, normalize = FALSE)
```

## Arguments

- var1:

  Numeric vector of values in `[0, 1]`.

- var2:

  Numeric vector of values in `[0, 1]`, same length as `var1`.

- n_bins:

  Integer; number of bins for discretisation. Defaults to `10`.

- normalize:

  Logical; if `TRUE`, normalises MI by the joint entropy. Defaults to
  `FALSE`.

## Value

Numeric; the mutual information value, or `NA` if any input values are
`NA`.
