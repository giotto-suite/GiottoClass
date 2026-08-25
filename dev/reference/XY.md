# Spatial coordinates

Directly get and set the xy(z) coordinates of spatial subobjects
(currently `spatLocsObj`, `giottoPoints`, `giottoPolygon`). coordinate
values are retrieved and set as `matrix`.

## Usage

``` r
# S4 method for class 'spatLocsObj'
XY(x, ...)

# S4 method for class 'spatLocsObj,matrix'
XY(x) <- value

# S4 method for class 'giottoPoints'
XY(x, ...)

# S4 method for class 'giottoPoints,ANY'
XY(x, ...) <- value

# S4 method for class 'giottoPolygon'
XY(x, ...)

# S4 method for class 'giottoPolygon,ANY'
XY(x, ...) <- value

# S4 method for class 'SpatVector'
XY(x, include_geom = FALSE, ...)

# S4 method for class 'SpatVector,matrix'
XY(x, geomtype = "points", ...) <- value
```

## Arguments

- x:

  object

- ...:

  additional args to pass

- value:

  matrix. xy(z) coordinates to set

- include_geom:

  logical. Whether `geom`, `part`, and `hole` from the terra geometry
  matrix should be included.

- geomtype:

  character. Either `"points"` or `"polygons"`. Only used with the
  `SpatVector` replacement method

## Value

`XY()` returns `matrix`. `XY<-()` returns same class as `x`

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

m1 <- XY(sl)
plot(sl)
XY(sl) <- m1 + 1000
plot(sl)

m2 <- XY(gpoints)
plot(gpoints)
XY(gpoints) <- m2 * 2 + 1000
plot(gpoints)

m3 <- XY(gpoly)
plot(gpoly)
XY(gpoly) <- m3 / 2
plot(gpoly)

XY(gpoly[1:10]) # vertices from first 10 polys
```
