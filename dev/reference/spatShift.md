# Spatially shift an object

Shift the spatial locations of an object

## Usage

``` r
# S4 method for class 'giotto'
spatShift(
  x,
  dx = 0,
  dy = 0,
  spat_unit = ":all:",
  feat_type = ":all:",
  images = ":all:"
)

# S4 method for class 'SpatExtent'
spatShift(x, dx = 0, dy = 0)

# S4 method for class 'spatLocsObj'
spatShift(x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE, ...)

# S4 method for class 'data.frame'
spatShift(
  x,
  dx = 0,
  dy = 0,
  dz = 0,
  copy_obj = TRUE,
  geom = c("sdimx", "sdimy", "sdimz"),
  ...
)

# S4 method for class 'giottoPolygon'
spatShift(x, dx = 0, dy = 0, copy_obj = FALSE, ...)

# S4 method for class 'giottoPoints'
spatShift(x, dx = 0, dy = 0, copy_obj = FALSE, ...)

# S4 method for class 'giottoLargeImage'
spatShift(x, dx = 0, dy = 0, copy_obj = FALSE, ...)

# S4 method for class 'giottoImage'
spatShift(x, dx = 0, dy = 0, ...)

# S4 method for class 'giottoAffineImage'
spatShift(x, dx = 0, dy = 0, ...)

# S4 method for class 'affine2d'
spatShift(x, dx = 0, dy = 0, ...)
```

## Arguments

- x:

  object

- dx:

  numeric. The shift on the x axis

- dy:

  numeric. The shift on the y axis

- spat_unit:

  character vector. spatial units to affect

- feat_type:

  character vector. feature types to affect

- images:

  character vector. Images to affect.

- dz:

  numeric. The shift on the z axis

- copy_obj:

  Default = TRUE

- ...:

  additional params to pass to methods

- geom:

  character. Named vector of colnames of x, y, (z) coordinate columns.
  Default is `c("sdimx", "sdimy", "sdimz")`

## Value

object with shifted spatial locations

## Details

With the `giotto` object, the ":all:" token can be passed to
`spat_unit`, `feat_type`, and `images` arguments to affect all available
items.

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

spatShift(g)
```
