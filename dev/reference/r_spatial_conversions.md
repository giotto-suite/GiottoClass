# R spatial conversions

R spatial conversions

## Usage

``` r
# S4 method for class 'sf'
as.sp(x)

# S4 method for class 'SpatVector'
as.sp(x)

# S4 method for class 'stars'
as.sp(x)

# S4 method for class 'Spatial'
as.sp(x)

# S4 method for class 'giottoPolygon'
as.sp(x, drop = TRUE)

# S4 method for class 'giottoPoints'
as.sp(x, drop = TRUE)

# S4 method for class 'SpatVector'
as.sf(x)

# S4 method for class 'Spatial'
as.sf(x)

# S4 method for class 'stars'
as.sf(x)

# S4 method for class 'sf'
as.sf(x)

# S4 method for class 'giottoPolygon'
as.sf(x, drop = TRUE)

# S4 method for class 'giottoPoints'
as.sf(x, drop = TRUE)

# S4 method for class 'SpatVector'
as.stars(x)

# S4 method for class 'sf'
as.stars(x)

# S4 method for class 'Spatial'
as.stars(x)

# S4 method for class 'stars'
as.stars(x)

# S4 method for class 'giottoPolygon'
as.stars(x, drop = TRUE)

# S4 method for class 'giottoPoints'
as.stars(x, drop = TRUE)

# S4 method for class 'SpatVector'
as.terra(x)

# S4 method for class 'sf'
as.terra(x)

# S4 method for class 'stars'
as.terra(x, type = c("vector", "raster"))

# S4 method for class 'Spatial'
as.terra(x)

# S4 method for class 'giottoPolygon'
as.terra(x, drop = TRUE)

# S4 method for class 'giottoPoints'
as.terra(x, drop = TRUE)
```

## Arguments

- x:

  The object to coerce

- drop:

  When TRUE, returned object will be of the desired object type instead
  of wrapped in a `giottoPoints` or `giottoPolygon` object

- type:

  whether data is 'vector' or 'raster'

## Value

sf, sp, stars or terra

## See also

Other As coercion functions:
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md),
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

as.sf(g)
```
