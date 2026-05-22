# Combine objects by rows (Giotto-related)

row bind two objects

## Usage

``` r
# S4 method for class 'giottoBinPoints,giottoBinPoints'
rbind2(x, y, ...)

# S4 method for class 'cellMetaObj,cellMetaObj'
rbind2(x, y, ...)

# S4 method for class 'featMetaObj,featMetaObj'
rbind2(x, y, ...)

# S4 method for class 'spatLocsObj,spatLocsObj'
rbind2(x, y, ...)

# S4 method for class 'giottoPolygon,giottoPolygon'
rbind2(x, y, add_list_ID = TRUE, ...)

# S4 method for class 'giottoPoints,giottoPoints'
rbind2(x, y, ...)

# S4 method for class 'overlapPointDT,overlapPointDT'
rbind2(x, y, ...)
```

## Arguments

- x:

  item 1 to rbind

- y:

  item 2 to rbind

- ...:

  additional params to pass to methods

- add_list_ID:

  whether to generate a list_ID column when giottoPolygons to append
  have different names

## Value

object with appended rows

## Functions

- `rbind2(x = giottoPolygon, y = giottoPolygon)`: Append giottoPolygon
  objects

- `rbind2(x = giottoPoints, y = giottoPoints)`: Append giottoPoints
  objects

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

rbind2(g, g)
#> [rbind giottoPolygon] Overlap information removed.
#>  Please recalculate with calculateOverlap() if needed.
#> An object of class giottoPolygon
#> spat_unit : "aggregate"
#> Spatial Information:
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 924, 4  (geometries, attributes)
#> extent      : 6391.466, 6903.573, -5153.897, -4694.868  (xmin, xmax, ymin, ymax)
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
