# Get feature info

Get giotto points spatVector

## Usage

``` r
getFeatureInfo(
  gobject = gobject,
  feat_type = NULL,
  return_giottoPoints = FALSE,
  set_defaults = TRUE,
  simplify = TRUE
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- return_giottoPoints:

  return as a giottoPoints object

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- simplify:

  logical. Whether or not to take object out of a list when there is a
  length of 1.

## Value

giotto points spatVector

## See also

Other feature info data accessor functions:
[`setFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/setFeatureInfo.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

getFeatureInfo(g)
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 79761, 3  (geometries, attributes)
#> extent      : 6400.037, 6900.032, -5149.983, -4699.979  (xmin, xmax, ymin, ymax)
#> source      : rna_feature_spatVector.shp
#> coord. ref. : 
#> names       : feat_ID global_z feat_ID_uniq
#> type        :   <chr>    <num>        <num>
#> values      :    Mlc1        0            1
#>                Gprc5b        0            2
#>                  Gfap        0            3
#>               ...
```
