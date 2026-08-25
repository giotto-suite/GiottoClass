# Do giottoPolygon

giottoPolygon objects carry 3 pieces of spatial information. The
polygons, their centroids, and the polygon overlapped features. All of
these need to be updated when spatial manipulations are applied. This
function simplifies performing functions on all SpatVector-based slots.

## Usage

``` r
.do_gpoly(x, what, args = NULL)
```

## Arguments

- x:

  giottoPolygon

- what:

  a call to do

- args:

  a `list` of additional args

## Value

giottoPolygon
