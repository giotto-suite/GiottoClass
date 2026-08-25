# Get nearest neighbor network

Get a NN-network from a Giotto object

## Usage

``` r
getNearestNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  ...
)

# S4 method for class 'giotto'
getNearestNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  nn_type = NULL,
  output = c("nnNetObj", "igraph", "data.table"),
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of NN network to be used

- ...:

  additional params to pass

- nn_type:

  "kNN" or "sNN"

- output:

  return a giotto `nnNetObj`, `igraph`, `data.table` object. Default
  'nnNetObj'

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

Giotto `nnNetObj`, `igraph` or `data.table` object

## See also

Other expression space nearest network accessor functions:
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

getNearestNetwork(gobject = g)
```
