# Add giotto polygons to giotto object

Adds Giotto polygon to an existing Giotto object

## Usage

``` r
addGiottoPolygons(gobject, gpolygons)
```

## Arguments

- gobject:

  giotto object

- gpolygons:

  list of giotto polygon objects, see
  [`createGiottoPolygonsFromMask`](https://giotto-suite.github.io/GiottoClass/reference/createGiottoPolygon.md)
  and
  [`createGiottoPolygonsFromDfr`](https://giotto-suite.github.io/GiottoClass/reference/createGiottoPolygon.md)

## Value

giotto object

## Examples

``` r
x <- GiottoData::loadSubObjectMini("giottoPolygon")
g <- createGiottoObject()
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

g <- addGiottoPolygons(gobject = g, gpolygons = list(x))
```
