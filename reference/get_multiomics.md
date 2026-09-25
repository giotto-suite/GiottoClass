# Get multiomics integration results

Get a multiomics integration result from a Giotto object

## Usage

``` r
get_multiomics(
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
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/setMultiomics.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/set_multiomics.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g <- setMultiomics(
    gobject = g, result = matrix(rnorm(100), nrow = 10),
    spat_unit = "cell", feat_type = "rna_protein"
)

get_multiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
#>              [,1]        [,2]        [,3]        [,4]        [,5]        [,6]
#>  [1,] -0.29697270 -0.01955325 -1.84926159  0.89130758 -2.70348138 -0.22344426
#>  [2,]  0.13122693 -0.46762272  0.56145707  1.62758122  0.29038123 -0.49857582
#>  [3,] -1.56495310 -0.16431026 -0.34883770 -1.23260939 -0.68073576 -0.12481335
#>  [4,] -0.82422792  1.69912784 -0.13678616  0.68610440  0.31861027  0.74445644
#>  [5,]  1.73397408 -1.14369864 -1.19301887 -2.45123127 -0.69780454  0.04993604
#>  [6,] -1.63798556 -0.12154774  0.03095702 -1.89533742  1.26014919 -0.70967190
#>  [7,]  0.40419912 -0.29105927  0.23291895  0.09237754  1.27863214  0.12774626
#>  [8,]  0.08658439 -0.24844333 -1.33143153 -0.39016194  1.36949166 -1.14230898
#>  [9,] -0.45354911 -0.11181289 -0.46128145  1.60620252  0.01060681  1.63509398
#> [10,] -0.54641552  0.23792348  0.08372638 -0.60965213 -0.67390639  1.36615949
#>              [,7]       [,8]        [,9]       [,10]
#>  [1,] -1.00090962  1.1981445 -0.67099396 -0.46362244
#>  [2,] -0.64903154 -0.7617763 -0.57209641 -0.25088119
#>  [3,] -0.08602487 -0.2857841  0.70215297 -0.11114805
#>  [4,]  1.29266467  0.2900724 -1.59741434 -0.06563187
#>  [5,]  0.70784522  0.0185031 -0.39518143  1.19006726
#>  [6,]  1.87810360 -0.7561942  1.75430380  0.04508842
#>  [7,] -0.03098862  0.9794807 -0.64609223 -1.89658438
#>  [8,] -0.07384704  1.6942495 -0.03878426 -0.10932417
#>  [9,]  0.15854783 -0.7301832 -0.68491767  0.94685966
#> [10,]  0.14186564 -0.8628747 -0.26056907 -0.62096816
```
