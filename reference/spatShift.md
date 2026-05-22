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

# S4 method for class 'spatialNetworkObj'
spatShift(x, dx = 0, dy = 0, dz = 0, copy_obj = TRUE, ...)

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

## Methods (by class)

- `spatShift(spatialNetworkObj)`: Shift the locations of a
  spatialNetworkObj

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

spatShift(g)
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
