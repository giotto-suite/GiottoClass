# Coerce to matrix

Coerce to matrix

## Usage

``` r
# S4 method for class 'spatLocsObj'
as.matrix(x, id_rownames = TRUE, ...)

# S4 method for class 'overlapPointDT'
as.matrix(x, feat_count_column = NULL, ...)

# S4 method for class 'overlapIntensityDT'
as.matrix(x, ...)

# S4 method for class 'nnNetObj'
as.matrix(x, attr = NULL, ...)
```

## Arguments

- x:

  object to coerce

- id_rownames:

  logical. Retain the spatial IDs as the rownames

- ...:

  additional params to pass (none implemented)

- feat_count_column:

  character. If provided, column in overlaps info that contains count
  information to take into account when generating matrix. This is
  important when point detections represent more than one count.

- attr:

  Either NULL or `character` providing the name of an edge attribute to
  include in the ajacency matrix. The edge attribute to use must be
  either `logical` or `numeric`.

## Value

matrix

## See also

Other As coercion functions:
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.points.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
m <- as.matrix(sl)
```
