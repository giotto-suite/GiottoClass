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
[`as.matrix`](https://giotto-suite.github.io/GiottoClass/reference/as.matrix.md),
[`as.points`](https://giotto-suite.github.io/GiottoClass/reference/as.points.md),
[`as.polygons`](https://giotto-suite.github.io/GiottoClass/reference/as.polygons.md),
[`r_spatial_conversions`](https://giotto-suite.github.io/GiottoClass/reference/r_spatial_conversions.md)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

data.table::as.data.table(g)
#>                                      poly_ID  stack agg_n valid
#>                                       <char> <char> <int> <int>
#>   1: 100210519278873141813371229408401071444   <NA>     2     1
#>   2: 101161259912191124732236989250178928032   <NA>     2     1
#>   3: 101488859781016188084173008420811094152   <NA>     2     1
#>   4: 101523780333017320796881555775415156847   <NA>     2     1
#>   5: 102184699197574201819246996094734116255   <NA>     2     1
#>  ---                                                           
#> 458:   9677424102111816817518421117250891895   <NA>     2     1
#> 459:  97068595823890802071024838798130036179   <NA>     2     1
#> 460:  97411078642927912684859796714759494710   <NA>     2     1
#> 461:   9816437869021910185567097363182418837   <NA>     2     1
#> 462:  98561957902191275233320065611022298397   <NA>     2     1
```
