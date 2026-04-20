# Standard deviation that returns 0 for vectors with fewer than two non-NA values

Standard deviation that returns 0 for vectors with fewer than two non-NA
values

## Usage

``` r
safe_sd(x, na.rm = TRUE)
```

## Arguments

- x:

  Numeric vector.

- na.rm:

  Logical; whether to remove `NA` values before computing. Defaults to
  `TRUE`.

## Value

Numeric; the standard deviation, or `0` if fewer than two non-`NA`
values are present.
