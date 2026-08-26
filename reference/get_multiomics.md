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
#>              [,1]        [,2]       [,3]        [,4]        [,5]        [,6]
#>  [1,] -0.05642605 -0.37221254 -0.4087357 -0.87151065  1.04067074  0.67326362
#>  [2,]  1.19142146  0.18263134 -2.2371918 -0.50174591  0.48370651  1.30851271
#>  [3,] -0.02745457  0.06055847  1.6845247  0.61345388 -1.38514216  1.77049183
#>  [4,]  0.88144485 -0.28844587  0.8478142 -2.27293071 -0.98822934  0.85102471
#>  [5,] -3.09168198  1.40416927 -1.4393261 -0.02903636 -0.81254630  0.92790215
#>  [6,]  1.12917805  1.96437541  0.7074581 -1.31878087 -0.03695966  0.33123211
#>  [7,]  1.96047642 -1.76916228  2.4144420 -1.59245568  0.12226510  0.76852894
#>  [8,]  0.24592419  0.11502054 -0.9043406 -0.15879473 -0.74855108  0.84241649
#>  [9,] -0.43052706 -0.47298787  0.4745312  0.39397965  0.09353390 -0.19412253
#> [10,] -0.28259708 -0.95256772 -0.6146149  0.01302273  1.73000527 -0.08600525
#>             [,7]       [,8]       [,9]      [,10]
#>  [1,]  0.8538184  0.6722306 -0.4924691  1.3420047
#>  [2,]  0.8218219  0.5891756  2.1452606 -0.2366189
#>  [3,] -0.8752590  1.4103935 -0.8097933  0.9920185
#>  [4,]  0.4488783  1.5924812 -0.7884894  0.5353546
#>  [5,] -0.6494303 -0.5777121  0.3992338  0.1432330
#>  [6,]  0.2674644 -2.8501235 -0.2241642 -0.5923419
#>  [7,]  1.1849021  0.2603700 -0.2484670  1.2831091
#>  [8,]  0.4669281 -1.0464900 -0.0543097 -1.1955548
#>  [9,] -0.5412949 -0.9433057  1.1699397  0.1398551
#> [10,] -0.7101682 -1.4025505  0.4040719 -0.4573394
```
