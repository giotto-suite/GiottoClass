# Rescale an object

Rescale an object spatially. Z dimension scaling is supported for some
types of subobjects.

## Usage

``` r
# S4 method for class 'giotto'
rescale(
  x,
  fx = 1,
  fy = fx,
  x0,
  y0,
  spat_unit = ":all:",
  feat_type = ":all:",
  images = ":all:"
)

# S4 method for class 'spatLocsObj'
rescale(x, fx = 1, fy = fx, fz = fx, x0, y0, z0)

# S4 method for class 'data.frame'
rescale(
  x,
  fx = 1,
  fy = fx,
  fz = fx,
  x0,
  y0,
  z0,
  geom = c("sdimx", "sdimy", "sdimz")
)

# S4 method for class 'giottoPolygon'
rescale(x, fx = 1, fy = fx, x0, y0)

# S4 method for class 'giottoPoints'
rescale(x, fx = 1, fy = fx, x0, y0)

# S4 method for class 'giottoImage'
rescale(x, fx = 1, fy = fx, x0, y0)

# S4 method for class 'giottoLargeImage'
rescale(x, fx = 1, fy = fx, x0, y0)

# S4 method for class 'giottoAffineImage'
rescale(x, fx = 1, fy = fx, x0, y0)

# S4 method for class 'affine2d'
rescale(x, fx = 1, fy = fx, x0, y0)
```

## Arguments

- x:

  object

- fx:

  numeric \> 0. The horizontal scaling factor

- fy:

  numeric \> 0. The vertical scaling factor

- x0:

  numeric. x-coordinate of the center of rescaling. If missing, the
  center of the extent of x is used

- y0:

  numeric. y-coordinate of the center of rescaling. If missing, the
  center of the extent of x is used

- spat_unit:

  character vector. spatial units to affect

- feat_type:

  character vector. feature types to affect

- images:

  character vector. Images to affect

- fz:

  numeric \> 0. The z scaling factor (only for supported objects)

- z0:

  numeric. z-coordinate of the center of rescaling. If missing, the
  center of the extent of x is used (only for supported objects)

- geom:

  character. Named vector of colnames of x, y, (z) coordinate columns.
  Default is `c("sdimx", "sdimy", "sdimz")`

## Value

re-scaled object

## Details

With the `giotto` object, the ":all:" token can be passed to
`spat_unit`, `feat_type`, and `images` arguments to affect all available
items.

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

rescale(g)
```
