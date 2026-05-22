# smoothGiottoPolygons

Smooths Giotto polygon object

## Usage

``` r
smoothGiottoPolygons(
  gpolygon,
  vertices = 20,
  k = 3,
  set_neg_to_zero = TRUE,
  ...
)
```

## Arguments

- gpolygon:

  giotto polygon object

- vertices:

  number of vertices

- k:

  k

- set_neg_to_zero:

  set negative values to zero (default: TRUE)

- ...:

  additional params to pass to `spline`

## Value

Smoothed Giotto polygon object with reduced vertices

## See also

[`spline`](https://rdrr.io/r/stats/splinefun.html)

## Examples

``` r
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")

smoothGiottoPolygons(gpolygon = gpoly)
#> An object of class giottoPolygon
#> spat_unit : "aggregate"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 462, 4  (geometries, attributes)
#> extent      : 6391.72, 6903.433, 0, 0  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> names       :                   poly_ID stack agg_n valid
#> type        :                     <chr> <chr> <int> <int>
#> values      : 100210519278873141813371~    NA     2     1
#>               101161259912191124732236~    NA     2     1
#>               101488859781016188084173~    NA     2     1
#>               ...
#>  centroids   : calculated
#>  overlaps    : NULL
```
