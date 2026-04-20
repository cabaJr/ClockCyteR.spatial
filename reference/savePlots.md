# Save a named list of ggplot objects to files

Save a named list of ggplot objects to files

## Usage

``` r
savePlots(
  obj_to_save,
  basepath,
  filename = "",
  extension,
  p.width = 560,
  p.height = 1100
)
```

## Arguments

- obj_to_save:

  Named list of `ggplot` objects to save.

- basepath:

  Character; directory where plots will be saved.

- filename:

  Character; optional prefix for the output filenames. Defaults to `""`.

- extension:

  Character; file extension / device type (e.g. `"svg"`, `"png"`).

- p.width:

  Numeric; output width in pixels. Defaults to `560`.

- p.height:

  Numeric; output height in pixels. Defaults to `1100`.

## Value

Called for its side effect of writing files; returns `NULL` invisibly.
