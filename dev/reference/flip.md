# Flip an object

Flip an object over a designated x or y value depending on direction
param input. Note that this behavior may be different from terra's

## Usage

``` r
# S4 method for class 'giotto'
flip(
  x,
  direction = "vertical",
  x0 = 0,
  y0 = 0,
  spat_unit = ":all:",
  feat_type = ":all:",
  ...
)

# S4 method for class 'giottoPolygon'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'giottoPoints'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'spatLocsObj'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'giottoLargeImage'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'SpatExtent'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'giottoAffineImage'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'affine2d'
flip(x, direction = "vertical", x0 = 0, y0 = 0)
```

## Arguments

- x:

  object

- direction:

  character. Direction to flip. Should be either partial match to
  'vertical' or 'horizontal'

- x0:

  x value to flip horizontally over (ignored for vertical). Pass NULL to
  flip over the extent

- y0:

  y value to flip vertically over (ignored for horizontal). Pass NULL to
  flip over the extent

- spat_unit:

  character vector. spatial units to affect

- feat_type:

  character vector. feature types to affect

- ...:

  additional args to pass

## Value

flipped object

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

flip(g)
```
