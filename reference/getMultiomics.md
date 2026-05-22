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
#>  active environment : 'giotto_env'
#>  python version : 3.10
g <- setMultiomics(
    gobject = g, result = matrix(rnorm(100), nrow = 10),
    spat_unit = "cell", feat_type = "rna_protein"
)

getMultiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
#>             [,1]        [,2]        [,3]        [,4]        [,5]       [,6]
#>  [1,] -1.7947501 -0.54314989  0.86164700 -0.36930863 -0.23013339  1.1884979
#>  [2,] -1.5936760 -0.11165896  0.88285986 -0.10648446  1.82750672 -0.6211862
#>  [3,] -0.5880921 -0.59879136  0.25782948 -0.52105633  1.63416402 -0.4248148
#>  [4,] -2.4436502 -0.64283303 -2.41919619 -0.31239384 -0.30347819 -1.2693983
#>  [5,]  0.8733072 -2.18494492  0.98255604 -1.73132386  0.05761964 -0.2533625
#>  [6,] -1.5700947  0.11409361  0.73347719  1.46947149 -0.50750744 -0.1116180
#>  [7,]  1.2064433  0.01148282  0.76860157  0.01044213 -1.61633811 -0.1315426
#>  [8,] -0.6020315  0.35329552 -0.01777067  0.40827683  0.89181656  0.8725992
#>  [9,]  0.6160790  2.29670311 -1.10088452  1.70418504 -0.03268551 -0.1657629
#> [10,] -0.2994089  0.61040765  0.21277709 -0.43014614 -0.16986547 -1.2409544
#>              [,7]       [,8]       [,9]      [,10]
#>  [1,]  1.47605219 -1.9719497  1.7008620 -1.1675388
#>  [2,] -0.07589098 -0.4848200 -0.4662269 -0.7923482
#>  [3,]  0.62842700 -1.3848879 -0.3168777 -0.3003550
#>  [4,]  0.19674511 -0.1762610 -1.2131392  0.1794639
#>  [5,]  0.08158049  0.2596137  0.2466786 -0.9374372
#>  [6,] -0.89830298 -0.2510197  0.1118254  1.4479660
#>  [7,] -0.61990929 -1.0445444 -1.0078516  1.3361280
#>  [8,] -1.06214909 -2.0920704  1.2416995  0.4995538
#>  [9,]  0.78311754 -0.5391583  1.3815616 -2.7557557
#> [10,] -2.78007372 -1.5206390 -2.5197112  0.4713136
```
