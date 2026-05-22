# centroids-generic

Access centroids information from polygon objects

## Usage

``` r
# S4 method for class 'giottoPolygon'
centroids(x, append_gpolygon = FALSE)
```

## Arguments

- x:

  object

- append_gpolygon:

  whether to append the centroids results to the `giottoPolygon` instead
  of returning bare `SpatVector`. Defaults to FALSE

## Value

spatVectorCentroids or spatVector

## Details

For giottoPolygon, if centroids already exist, pulls from
`spatVectorCentroids` slot. Otherwise, generates from `spatVector` slot
de novo

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

centroids(g)
#> class       : SpatVector
#> geometry    : points
#> dimensions  : 462, 4  (geometries, attributes)
#> extent      : 6401.412, 6899.108, -5146.747, -4700.326  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       :                   poly_ID stack agg_n valid
#> type        :                     <chr> <chr> <int> <int>
#> values      : 100210519278873141813371~    NA     2     1
#>               101161259912191124732236~    NA     2     1
#>               101488859781016188084173~    NA     2     1
#>               ...
```
