# Get spatial network

Function to get a spatial network

## Usage

``` r
getSpatialNetwork(gobject, spat_unit = NULL, name = NULL, ...)

# S4 method for class 'giotto'
getSpatialNetwork(
  gobject,
  spat_unit = NULL,
  name = NULL,
  output = c("spatialNetworkObj", "igraph", "networkDT", "unfiltered", "outputObj"),
  set_defaults = TRUE,
  copy_obj = TRUE,
  verbose = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- name:

  name of spatial network

- ...:

  additional params to pass

- output:

  object type to return as. Options: 'spatialNetworkObj' (default),
  'networkDT' and 'networkDT_before_filter' for data.table outputs.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- copy_obj:

  whether to copy/duplicate when getting the object (default = TRUE)

- verbose:

  be verbose

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

spatialNetworkObj of data.table

## See also

Other spatial network data accessor functions:
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md)

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
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

getSpatialNetwork(g)
```
