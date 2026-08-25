# Set multiomics integration results

Set a multiomics integration result in a Giotto object

## Usage

``` r
set_multiomics(gobject, ...)
```

## Arguments

- gobject:

  A Giotto object

- spat_unit:

  spatial unit (e.g. 'cell')

- feat_type:

  (e.g. 'rna_protein')

- result:

  A matrix or result from multiomics integration (e.g. theta weighted
  values from runWNN)

- integration_method:

  multiomics integration method used. Default = 'WNN'

- result_name:

  Default = 'theta_weighted_matrix'

- verbose:

  be verbose

## Value

A giotto object

## See also

Other multiomics accessor functions:
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getMultiomics.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/get_multiomics.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md)

Other functions to set data in giotto object:
[`setCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setCellMetadata.md),
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setDimReduction.md),
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setExpression.md),
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureInfo.md),
[`setFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setFeatureMetadata.md),
[`setGiotto,giotto,giottoBinPoints-method`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md),
[`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiottoImage.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setMultiomics.md),
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md),
[`setPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setPolygonInfo.md),
[`setSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialEnrichment.md),
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialGrid.md),
[`setSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialLocations.md),
[`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

set_multiomics(
    gobject = g, x = matrix(rnorm(100), nrow = 10),
    spat_unit = "cell", feat_type = "rna_protein"
)
```
