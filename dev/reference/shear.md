# Apply a shear tranform

Apply shear transformation to a spatial object. Currently only works for
2D transforms. This implementation applies a shear along one axis by
adding the value of the other axis after a multiplicative factor `fx` or
`fy` is applied.

## Usage

``` r
# S4 method for class 'spatLocsObj'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'SpatVector'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'giottoPoints'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'giottoPolygon'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'giottoLargeImage'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'giottoAffineImage'
shear(x, fx = 0, fy = 0, x0, y0, ...)

# S4 method for class 'affine2d'
shear(x, fx = 0, fy = 0, x0, y0, ...)
```

## Arguments

- x:

  object

- fx:

  numeric. x shear

- fy:

  numeric. y shear

- x0:

  numeric. x-origin of shear

- y0:

  numeric. y-origin of shear

- ...:

  additional args to pass (none implemented)

## Value

shear transformed object

## Examples

``` r
sl <- GiottoData::loadSubObjectMini("spatLocsObj")

plot(shear(sl, fx = 2))

# equivalent affine transform
shear_m <- diag(rep(1, 3))
shear_m[2, 1] <- 2
plot(affine(sl, shear_m))
plot(shear(sl, fx = 2, x0 = 0, y0 = 0))
```
