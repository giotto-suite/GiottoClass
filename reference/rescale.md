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
#> An object of class spatLocsObj : "raw"
#> spat_unit : "aggregate"
#> provenance: z0 z1 
#> dimensions: 462 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6637.881 -5140.465 100210519278873141813371229408401071444
#> 2: 6471.978 -4883.541 101161259912191124732236989250178928032
#> 3: 6801.610 -4968.685 101488859781016188084173008420811094152
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6401.412 -5146.747
#> [2,] 6899.108 -4700.326
#> 
```
