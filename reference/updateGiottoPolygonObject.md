# Update giotto polygon object

Update giotto polygon object

## Usage

``` r
updateGiottoPolygonObject(gpoly)
```

## Arguments

- gpoly:

  giotto polygon object

## Value

GiottoPolygonObject

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

updateGiottoPolygonObject(g)
#> An object of class giottoPolygon
#> spat_unit : "aggregate"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 462, 4  (geometries, attributes)
#> extent      : 6391.466, 6903.573, -5153.897, -4694.868  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       :                   poly_ID stack agg_n valid
#> type        :                     <chr> <chr> <int> <int>
#> values      : 100210519278873141813371~    NA     2     1
#>               101161259912191124732236~    NA     2     1
#>               101488859781016188084173~    NA     2     1
#>               ...
#>  centroids   : calculated
#>  overlaps    : rna
```
