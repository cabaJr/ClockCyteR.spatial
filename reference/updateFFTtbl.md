# Append new FFT results to an accumulator list

Append new FFT results to an accumulator list

## Usage

``` r
updateFFTtbl(old_list, new_list, filename)
```

## Arguments

- old_list:

  List; existing accumulator as returned by
  [`createFFTtbl()`](https://cabajr.github.io/ClockCyteR.spatial/reference/createFFTtbl.md).

- new_list:

  List; new results to append; must contain elements `trace_no` and
  `trace_tot`.

- filename:

  Character; file identifier added to `analysed_names_vec`.

## Value

Updated accumulator list.
