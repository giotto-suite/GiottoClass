# Get multiomics integration results

Get a multiomics integration result from a Giotto object

## Usage

``` r
getMultiomics(gobject, spat_unit = NULL, feat_type = NULL, ...)

# S4 method for class 'giotto'
getMultiomics(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  integration_method = "WNN",
  result_name = "theta_weighted_matrix"
)
```

## Arguments

- gobject:

  A Giotto object

- spat_unit:

  spatial unit (e.g. 'cell')

- feat_type:

  integrated feature type (e.g. 'rna_protein')

- integration_method:

  multiomics integration method used. Default = 'WNN'

- result_name:

  Default = 'theta_weighted_matrix'

## Value

A multiomics integration result (e.g. theta_weighted_matrix from WNN)

## See also

Other multiomics accessor functions:
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_multiomics.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
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
g <- setMultiomics(
    gobject = g, x = matrix(rnorm(100), nrow = 10),
    spat_unit = "cell", feat_type = "rna_protein"
)

getMultiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
```
