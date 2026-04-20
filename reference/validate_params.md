# validate_params

Checks a parameter list for completeness and internal consistency,
stopping with an informative error if any required fields are missing or
invalid.

## Usage

``` r
validate_params(p)
```

## Arguments

- p:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

## Value

Called for its side effects. Returns `TRUE` invisibly if all checks
pass.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_params(params)
} # }
```
