# Remove outliers, smooth, and detrend fluorescence traces

Remove outliers, smooth, and detrend fluorescence traces

## Usage

``` r
preprocess_data(data, grade, mode = "mov_avg", parallel = FALSE)
```

## Arguments

- data:

  Numeric matrix of fluorescence traces with a leading time column (time
  × cells).

- grade:

  Integer; polynomial degree used for detrending.

- mode:

  Character; smoothing method — currently only `"mov_avg"` (moving
  average) is supported. Defaults to `"mov_avg"`.

- parallel:

  Logical; reserved for future parallelisation. Defaults to `FALSE`.

## Value

A numeric matrix of the same dimensions as `data` with smoothed and
detrended traces.
