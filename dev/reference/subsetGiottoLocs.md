# Subset by spatial locations

Subsets Giotto object spatially by defining a set of cropping bounds.
The information to be subset is preferred to be from spatial locations.
If no `spat_loc_name` is given, the first available set of spatial
locations for the `spat_unit` will be picked. If no spatial locations
are available, the polygon information will be subset. Spatial IDs
surviving the crop are then applied to the rest of the Giotto object
using
[subsetGiotto](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiotto.md).

## Usage

``` r
subsetGiottoLocs(
  gobject,
  spat_unit = NULL,
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
  verbose = FALSE,
  toplevel_params = 5
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

  character. polygon information to subset (considered separately from
  the spat_units to subset)

- return_gobject:

  return Giotto object

- verbose:

  be verbose

- toplevel_params:

  parameters to extract

## Value

giotto object

## Details

If `return_gobject = FALSE`, then a filtered combined metadata
data.table will be returned

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

subsetGiottoLocs(g, x_max = 4000, y_max = -1000)
```
