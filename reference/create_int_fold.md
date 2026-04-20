# Create a subdirectory for an analysis interval

Create a subdirectory for an analysis interval

## Usage

``` r
create_int_fold(int, foldername)
```

## Arguments

- int:

  Named list element representing a single interval; the name is
  `"Full"` for whole-recording analyses or an arbitrary label for
  sub-intervals, and the value is a numeric vector `c(start, end)`.

- foldername:

  Character; base directory in which the interval subdirectory will be
  created.
