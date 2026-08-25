# Get dimension reduction

Function to get a dimension reduction object

## Usage

``` r
getDimReduction(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)

# S4 method for class 'giotto'
getDimReduction(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  reduction = c("cells", "feats"),
  reduction_method = NULL,
  output = c("dimObj", "matrix"),
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

  name of reduction results

- ...:

  additional params to pass

- reduction:

  reduction on cells or features (e.g. "cells", "feats")

- reduction_method:

  reduction method (e.g. "pca", "umap", "tsne")

- output:

  object type to return as. Either 'dimObj' (default) or 'matrix' of the
  embedding coordinates.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

dim reduction object (default) or dim reduction coordinates

## See also

Other dimensional reduction data accessor functions:
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
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
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

getDimReduction(g)
```
