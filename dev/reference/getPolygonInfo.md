# Get polygon info

Get giotto polygon spatVector

## Usage

``` r
getPolygonInfo(gobject, name = NULL, ...)

# S4 method for class 'giotto'
getPolygonInfo(
  gobject,
  name = NULL,
  polygon_overlap = NULL,
  return_giottoPolygon = FALSE,
  verbose = TRUE,
  simplify = TRUE,
  ...,
  polygon_name = deprecated()
)
```

## Arguments

- gobject:

  giotto object

- name:

  name of polygons. NULL (default) selects "cell" when available,
  otherwise the first available

- polygon_overlap:

  include polygon overlap information

- return_giottoPolygon:

  (Defaults to FALSE) Return as giottoPolygon S4 object

- verbose:

  be verbose

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

- polygon_name:

  deprecated. Use `name`

## Value

spatVector

## See also

Other polygon info data accessor functions:
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setPolygonInfo.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getNearestNetwork.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

getPolygonInfo(g)
```
