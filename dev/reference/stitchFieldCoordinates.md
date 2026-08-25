# stitchFieldCoordinates

Helper function to stitch field coordinates together to form one
complete picture

## Usage

``` r
stitchFieldCoordinates(
  location_file,
  offset_file,
  cumulate_offset_x = FALSE,
  cumulate_offset_y = FALSE,
  field_col = "Field of View",
  X_coord_col = "X",
  Y_coord_col = "Y",
  reverse_final_x = FALSE,
  reverse_final_y = TRUE
)
```

## Arguments

- location_file:

  location dataframe with X and Y coordinates

- offset_file:

  dataframe that describes the offset for each field (see details)

- cumulate_offset_x:

  (boolean) Do the x-axis offset values need to be cumulated?

- cumulate_offset_y:

  (boolean) Do the y-axis offset values need to be cumulated?

- field_col:

  column that indicates the field within the location_file

- X_coord_col:

  column that indicates the x coordinates

- Y_coord_col:

  column that indicates the x coordinates

- reverse_final_x:

  (boolean) Do the final x coordinates need to be reversed?

- reverse_final_y:

  (boolean) Do the final y coordinates need to be reversed?

## Value

Updated location dataframe with new X \['X_final'\] and Y \['Y_final'\]
coordinates

## Details

Stitching of fields:

1.  **have cell locations:** at least 3 columns: field, X, Y

2.  **create offset file:** offset file has 3 columns: field, x_offset,
    y_offset

3.  create new cell location file by stitching original cell locations
    with stitchFieldCoordinates

4.  provide new cell location file to
    [`createGiottoObject`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)
