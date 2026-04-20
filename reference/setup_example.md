# Copy the toy dataset to a writable location

Copy the toy dataset to a writable location

## Usage

``` r
setup_example(dest = file.path(tempdir(), "ClockCyteR_example"))
```

## Arguments

- dest:

  Character; directory where the toy dataset will be copied. Defaults to
  a `ClockCyteR_example` subdirectory inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Character; path to the copied dataset folder (invisibly).
