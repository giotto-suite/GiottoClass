# Get spatial locations

Function to get a spatial location data.table

## Usage

``` r
getSpatialLocations(gobject, spat_unit = NULL, name = NULL, ...)

# S4 method for class 'giotto'
getSpatialLocations(
  gobject,
  spat_unit = NULL,
  name = NULL,
  output = c("spatLocsObj", "data.table"),
  copy_obj = TRUE,
  verbose = TRUE,
  set_defaults = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial locations (defaults to first name in spatial_locs
  slot, e.g. "raw")

- ...:

  additional params to pass

- output:

  what object type to get the spatial locations as. Default is as a
  'spatLocsObj'. Returning as 'data.table' is also possible.

- copy_obj:

  whether to copy/duplicate when getting the object (default = TRUE)

- verbose:

  be verbose

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

data.table with coordinates or spatLocsObj depending on `output`

## See also

Other spatial location data accessor functions:
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

getSpatialLocations(g)
```
