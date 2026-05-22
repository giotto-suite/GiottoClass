# deprecated

deprecated

## Usage

``` r
subsetGiottoLocsMulti(
  gobject,
  spat_unit = ":all:",
  feat_type = NULL,
  feat_type_ssub = ":all:",
  spat_loc_name = NULL,
  x_max = NULL,
  x_min = NULL,
  y_max = NULL,
  y_min = NULL,
  z_max = NULL,
  z_min = NULL,
  poly_info = NULL,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit to subset

- feat_type:

  (optional) feat type to use if `return_gobject = TRUE` and a combined
  data.table output is desired.

- feat_type_ssub:

  which feat types across which to apply the spatial subset

- spat_loc_name:

  name of spatial locations to use within spat_unit

- x_max, x_min, y_max, y_min, z_max, z_min:

  minimum and maximum x, y, and z coordinates to subset to

- poly_info:

  named list of character vectors. names correspond to spat_unit and
  character vectors are the polygons associated with those spat units

- return_gobject:

  return Giotto object

- verbose:

  be verbose

## Value

giotto object
