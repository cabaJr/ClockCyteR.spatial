# prepare_file

prepare_file

## Usage

``` r
prepare_file(file_row, params)
```

## Arguments

- file_row:

  list of files to analyse, output of index_files function

- params:

  params object created by the make_params function

## Value

a list containing the file_id, the ROI, the coordinates of each grid
element, the coordinates of each point as sf elements, the path to the
main folder, and the channels to be analysed
