# Composite Interaction Score (CI) between two variables

Composite Interaction Score (CI) between two variables

## Usage

``` r
CI_score(tbl1, var1, tbl2, var2, weight = FALSE, merge = FALSE)
```

## Arguments

- tbl1:

  Data frame containing the first variable and an `ID` column.

- var1:

  Character; column name of the first variable in `tbl1`.

- tbl2:

  Data frame containing the second variable.

- var2:

  Character; column name of the second variable in `tbl2`.

- weight:

  Logical; if `TRUE`, weights the score by `1 - |var1 - var2|`. Defaults
  to `FALSE`.

- merge:

  Logical; reserved for future use. Defaults to `FALSE`.

## Value

A data frame with columns `ID` and `CI_score`.
