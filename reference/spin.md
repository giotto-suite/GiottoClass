# Spin an object

Spin (rotate) an object spatially (usually limited to xy rotations)

## Usage

``` r
# S4 method for class 'giotto'
spin(x, angle, x0 = NULL, y0 = NULL, spat_unit = ":all:", feat_type = ":all:")

# S4 method for class 'giottoPolygon'
spin(x, angle, x0 = NULL, y0 = NULL)

# S4 method for class 'giottoPoints'
spin(x, angle, x0 = NULL, y0 = NULL)

# S4 method for class 'spatLocsObj'
spin(
  x,
  angle = NULL,
  x0 = NULL,
  y0 = NULL,
  z0 = NULL,
  xy_angle = NULL,
  zy_angle = NULL,
  xz_angle = NULL
)

# S4 method for class 'data.frame'
spin(
  x,
  angle = NULL,
  x0 = NULL,
  y0 = NULL,
  z0 = NULL,
  xy_angle = NULL,
  zy_angle = NULL,
  xz_angle = NULL,
  geom = c("sdimx", "sdimy", "sdimz")
)

# S4 method for class 'giottoLargeImage'
spin(x, angle = NULL, x0 = NULL, y0 = NULL, ...)

# S4 method for class 'giottoAffineImage'
spin(x, angle = NULL, x0 = NULL, y0 = NULL, ...)

# S4 method for class 'affine2d'
spin(x, angle = NULL, x0 = NULL, y0 = NULL)
```

## Arguments

- x:

  object

- angle:

  numeric. Angle of rotation in degrees

- x0:

  numeric. x-coordinate of the center of rotation. Defaults to center x
  val if not given.

- y0:

  numeric. y-coordinate of the center of rotation. Defaults to center y
  val if not given.

- spat_unit:

  character vector. spatial units to affect

- feat_type:

  character vector. feature types to affect (giottoPoints only).

- z0:

  spatLocsObj specific. Numeric. z-coordinate of the center of rotation.
  Depending on if z data is present, defaults to either 0 or center z
  val if not given.

- xy_angle:

  spatLocsObj specific. xy plane rotation in degrees. Overrides angle
  param

- zy_angle:

  spatLocsObj specific. zy plane rotation

- xz_angle:

  spatLocsObj specific. xz plane rotation

- geom:

  character. Named vector of colnames of x, y, (z) coordinate columns.
  Default is `c("sdimx", "sdimy", "sdimz")`

- ...:

  additional params to pass

## Value

spun object

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

spin(g)
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
