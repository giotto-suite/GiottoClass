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
[`as.data.table.giottoBinPoints()`](https://giotto-suite.github.io/GiottoClass/reference/as.data.table.md),
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/reference/as.points.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/reference/as.polygons.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPoints")

as.sf(g)
#> Simple feature collection with 79761 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 6400.037 ymin: -5149.983 xmax: 6900.032 ymax: -4699.978
#> CRS:           NA
#> First 10 features:
#>    feat_ID global_z feat_ID_uniq                   geometry
#> 1     Mlc1        0            1 POINT (6400.037 -4966.651)
#> 2   Gprc5b        0            2 POINT (6400.041 -4965.377)
#> 3     Gfap        0            3 POINT (6400.078 -5081.453)
#> 4     Gfap        0            4 POINT (6400.084 -5038.288)
#> 5    Ednrb        0            5 POINT (6400.172 -4816.516)
#> 6     Gfap        0            6 POINT (6400.172 -5077.041)
#> 7     Gfap        0            7 POINT (6400.189 -5080.815)
#> 8     Sox9        0            8 POINT (6400.228 -4770.126)
#> 9     Gfap        0            9 POINT (6400.246 -5096.167)
#> 10  Gprc5b        0           10 POINT (6400.278 -4928.655)
```
