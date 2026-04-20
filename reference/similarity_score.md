# Compute a pairwise similarity score between two variables

Compute a pairwise similarity score between two variables

## Usage

``` r
similarity_score(tbl1, tbl2, var1, var2, key = "ID")
```

## Arguments

- tbl1:

  Data frame containing `var1` and `key`.

- tbl2:

  Data frame containing `var2` and `key`.

- var1:

  Character; column name of the first variable in `tbl1`.

- var2:

  Character; column name of the second variable in `tbl2`.

- key:

  Character; name of the shared ID column used to join the tables.
  Defaults to `"ID"`.

## Value

A data frame with the `key` column and a `similarity` column
(`1 - |var1 - var2|`).
