# Settle polygon bounds

Settle the boundaries between polygons when they overlap by splitting
both at the point where they touch. Works through intersection with the
voronoi of the centroids.

## Usage

``` r
# S4 method for class 'giottoPolygon'
settleGeom(x)

# S4 method for class 'SpatVector'
settleGeom(x)
```

## Arguments

- x:

  a `SpatVector` of type "polygons" or object inheriting from
  `giottoPolygon`

## Value

same class as `x`, with the contained polygons borders settled in
relation to each other.

## Examples

``` r
svp <- GiottoData::loadSubObjectMini("giottoPolygon")[]
svp <- buffer(svp, 5)
plot(svp)

svp <- settleGeom(svp)
plot(svp)
```
