# Import and scale grid centroid coordinates

Import and scale grid centroid coordinates

## Usage

``` r
get_grid_coords(folder, filename = "grid_centroids.csv", pixel_fct = 2.82)
```

## Arguments

- folder:

  Character; path to the folder containing the CSV file.

- filename:

  Character; name of the CSV file with grid centroids. Defaults to
  `"grid_centroids.csv"`.

- pixel_fct:

  Numeric; pixel-to-micrometre conversion factor. Defaults to `2.82`.

## Value

A data frame with columns `ID`, `x`, and `y` (coordinates in
micrometres).
