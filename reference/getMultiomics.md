# Get multiomics integration results

Get a multiomics integration result from a Giotto object

## Usage

``` r
getMultiomics(
  gobject = NULL,
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
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md),
[`setMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/setMultiomics.md),
[`set_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/set_multiomics.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

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

getMultiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
#>              [,1]        [,2]       [,3]       [,4]        [,5]        [,6]
#>  [1,] -1.54728667 -0.15323120 -0.2880079  0.1504491  0.15113150  2.35584224
#>  [2,]  0.15626219 -0.21303912 -0.9015857 -0.4378595 -0.47971217 -0.07829679
#>  [3,] -1.04072862  0.02506200  1.4179229  0.9804014  1.37373240 -2.48334004
#>  [4,] -0.11467107  1.96181765 -0.5921200 -0.8170110  0.45401113  0.40703514
#>  [5,] -0.09855060 -0.29303855 -0.8486003 -0.2404877  0.66119987 -2.09087675
#>  [6,]  0.58468086  0.58223044 -1.5604810  0.7612364 -0.33980112 -0.92110647
#>  [7,]  1.81903015  0.29278767  0.3805031  0.7141979  0.46630398  0.59907492
#>  [8,]  0.02010859 -0.04028543  0.1234657  0.6746769 -0.06975380 -0.43413197
#>  [9,] -1.36697635 -0.21435764 -0.6939894 -0.9004623 -0.09697841  0.83620429
#> [10,]  0.62464581  1.54624238  1.3994524 -0.7682120  0.92150179 -0.72578170
#>              [,7]        [,8]       [,9]      [,10]
#>  [1,]  0.60109578  0.21107021 -0.1929415 -0.4518673
#>  [2,]  0.09118194 -1.10175805  1.2685203  0.2796521
#>  [3,] -0.32197855  0.39341368  1.6365113 -0.9323407
#>  [4,] -2.12245121  1.82474216 -0.7338841  0.4217614
#>  [5,] -0.34599898  1.26347435 -1.1241179  0.8055708
#>  [6,] -0.48001955  1.68331051 -0.4216800 -0.7100369
#>  [7,] -0.42998550  0.95805295  0.4735953  0.3198127
#>  [8,] -2.07498956  1.60617521  0.9768600  0.9921510
#>  [9,]  0.11144165  0.07428736 -0.3543777 -0.4675122
#> [10,] -1.15421265  1.97654017 -0.6908579 -0.2283285
```
