# Rotate spatial locations

Rotate given spatlocs by given radians

## Usage

``` r
.rotate_spatial_locations(
  spatlocs,
  rotateradians = c(xy = 0, zy = 0, xz = 0),
  rcenter = c(0, 0, 0),
  geom = c("sdimx", "sdimy", "sdimz")
)
```

## Arguments

- spatlocs:

  spatial locations to use

- rotateradians:

  Named vector of radians for rotation along each of the 3 coordinate
  axes. If only a single value is provided, it will be treated as xy
  rotation.

- rcenter:

  center of rotation given as vector xy(z) coordinates (defaults to
  coordinate center)

- geom:

  character. Named vector of colames of x, y, z coordinate columns.
  Default is `c("sdimx", "sdimy", "sdimz")`

## Value

spatial locations

## Details

Radians are provided through `rotateradians` param as a named vector
with values for `xy` (yaw), `zy` (pitch), `xz` (roll)
