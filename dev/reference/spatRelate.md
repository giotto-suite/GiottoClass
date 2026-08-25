# Spatial relationship as a filter

Narrow `x` to features that satisfy a spatial predicate against any
feature of `y`. Returns an object of the same class as `x` rather than a
relation matrix – the "filter form" complement to
[`relate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md).

## Usage

``` r
# S4 method for class 'giottoSpatial,giottoSpatial'
spatRelate(x, y, relation = "intersects", ...)
```

## Arguments

- x:

  spatial object to be narrowed (rows kept where predicate holds against
  any feature of `y`)

- y:

  query geometry; the form depends on the method (giottoSpatial,
  SpatVector, sf, character WKT)

- relation:

  `character`. Spatial predicate. One of `"intersects"`, `"touches"`,
  `"crosses"`, `"overlaps"`, `"within"`, `"contains"`, `"covers"`,
  `"covered_by"`, `"disjoint"`. Default `"intersects"`.

- ...:

  additional args to pass

## Value

an object of the same class as `x`, narrowed to features satisfying the
predicate against any feature of `y`

## See also

[`relate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md)
for the relation-matrix / pairs form;
[`spatQuery()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
for the gobject-level multi-filter pipeline.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
gpoly <- g[["spatial_info"]][[1]]
gpoints <- g[["feat_info"]][[1]]

# narrow points to those that intersect at least one polygon
pts_in_polys <- spatRelate(gpoints, gpoly, relation = "intersects")
```
