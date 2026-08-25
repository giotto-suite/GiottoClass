# Coerce to data.table

Coerce to data.table if possible

## Usage

``` r
# S3 method for class 'giottoBinPoints'
as.data.table(x, geom, ...)

# S3 method for class 'SpatVector'
as.data.table(
  x,
  keep.rownames = FALSE,
  geom = NULL,
  include_values = TRUE,
  geomtype,
  ...
)

# S3 method for class 'giottoPolygon'
as.data.table(x, ...)

# S3 method for class 'giottoPoints'
as.data.table(x, ...)

# S3 method for class 'overlapPointDT'
as.data.frame(x, ...)

# S3 method for class 'overlapIntensityDT'
as.data.frame(x, ...)
```

## Arguments

- x:

  The object to coerce

- geom:

  character or NULL. If not NULL, either "XY", "WKT", or "HEX", to get
  the geometry included in coordinates of each point or vertex,
  Well-Known-Text or hexadecimal notation.

- ...:

  additional arguments to pass

- keep.rownames:

  This argument is ignored

- include_values:

  whether to include attributes information when geom is 'XY'

- geomtype:

  character (optional). One of "points" or "polygons". Fallback geomtype
  used when it is not possible for {terra} to determine the type of
  geometry an object is. (commonly seen when nrow of the object = 0)

## Value

data.table

## See also

Other As coercion functions:
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

data.table::as.data.table(g)
```
