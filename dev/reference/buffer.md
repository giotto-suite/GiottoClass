# Create a buffer around vector geometries

Calculate a buffer around all geometries of a `SpatVector`

## Usage

``` r
# S4 method for class 'spatLocsObj'
buffer(x, width, ..., settle = TRUE)

# S4 method for class 'giottoPoints'
buffer(x, width, ..., settle = TRUE)

# S4 method for class 'giottoPolygon'
buffer(x, width, ..., settle = TRUE)
```

## Arguments

- x:

  SpatRaster or SpatVector

- width:

  numeric. Unit is meter if `x` has a longitude/latitude CRS, or in the
  units of the coordinate reference system in other cases (typically
  also meter). The value should be \> 0 if `x` is a SpatRaster. If `x`
  is a SpatVector, this argument is vectorized, meaning that you can
  provide a different value for each geometry in `x`; and you can also
  use the name of a variable in `x` that has the widths

- ...:

  additional params to pass

- settle:

  logical. Settle the borders between polygons by cutting them where
  they touch based on voronoi boundaries.

## Value

`giottoPolygon` of buffer polygons

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
slb <- buffer(sl, 30)
plot(slb)
```
