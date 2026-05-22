# showGiottoFeatInfo

show the available giotto spatial feature information

## Usage

``` r
showGiottoFeatInfo(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

SpatVector

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoExpression.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatialInfo.md)

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

showGiottoFeatInfo(g)
#> For Feature info:  rna 
#> 
#> An object of class giottoPoints
#> feat_type : "rna"
#> Feature Information:
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
#> 
#> -----------------------------
#>  
```
