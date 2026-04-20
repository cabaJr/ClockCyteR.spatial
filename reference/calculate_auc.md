# Calculate the area under a fluorescence trace

Calculate the area under a fluorescence trace

## Usage

``` r
calculate_auc(y, normalize = TRUE)
```

## Arguments

- y:

  Numeric vector of fluorescence values.

- normalize:

  Logical; if `TRUE`, divides the AUC by the number of time points.
  Defaults to `TRUE`.

## Value

Numeric; the (optionally normalised) trapezoidal AUC.
