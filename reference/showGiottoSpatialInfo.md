# showGiottoSpatialInfo

show the available giotto spatial polygon information

## Usage

``` r
showGiottoSpatialInfo(gobject)
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
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatNetworks.md)

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

showGiottoSpatialInfo(g)
#> For Spatial info:  z0 
#> 
#> An object of class giottoPolygon
#> spat_unit : "z0"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 498, 1  (geometries, attributes)
#> extent      : 6399.244, 6903.243, -5152.39, -4694.868  (xmin, xmax, ymin, ymax)
#> source      : z0_spatInfo_spatVector.shp
#> coord. ref. : 
#> names       :                                 poly_ID
#> type        :                                   <chr>
#> values      :  40951783403982682273285375368232495429
#>               240649020551054330404932383065726870513
#>               274176126496863898679934791272921588227
#>               ...
#>  centroids   : calculated
#>  overlaps    : rna
#> -----------------------------
#>  
#> For Spatial info:  z1 
#> 
#> An object of class giottoPolygon
#> spat_unit : "z1"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 504, 1  (geometries, attributes)
#> extent      : 6391.466, 6903.573, -5153.897, -4694.873  (xmin, xmax, ymin, ymax)
#> source      : z1_spatInfo_spatVector.shp
#> coord. ref. : 
#> names       :                                 poly_ID
#> type        :                                   <chr>
#> values      :  40951783403982682273285375368232495429
#>                17685062374745280598492217386845129350
#>               223553142498364321238189328942498473503
#>               ...
#>  centroids   : calculated
#>  overlaps    : rna
#> -----------------------------
#>  
#> For Spatial info:  aggregate 
#> 
#> An object of class giottoPolygon
#> spat_unit : "aggregate"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 462, 4  (geometries, attributes)
#> extent      : 6391.466, 6903.573, -5153.897, -4694.868  (xmin, xmax, ymin, ymax)
#> source      : aggregate_spatInfo_spatVector.shp
#> coord. ref. : 
#> names       :                   poly_ID stack agg_n valid
#> type        :                     <chr> <chr> <num> <int>
#> values      : 100210519278873141813371~    NA     2     1
#>               101161259912191124732236~    NA     2     1
#>               101488859781016188084173~    NA     2     1
#>               ...
#>  centroids   : calculated
#>  overlaps    : rna
#> -----------------------------
#>  
```
