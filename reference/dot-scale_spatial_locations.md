# Scale spatial locations

Simple scaling of spatial locations by given `scale_factor`. Values will
be scaled from the coordinate origin or coordinates provided through
`scenter` param. Default values supply values for z axis, but these
values will only be applied if input `data.table` has z information as
detected by the third item in the `geom` param.

## Usage

``` r
.scale_spatial_locations(
  spatlocs,
  scale_factor = c(1, 1, 1),
  scenter = c(0, 0, 0),
  geom = c("sdimx", "sdimy", "sdimz")
)
```

## Arguments

- spatlocs:

  data.table. spatial locations information to scale

- scale_factor:

  scaling factor to apply to coordinates. Default is `c(1, 1, 1)`

- scenter:

  center from which to scale spatial coordinates. Given as vector of
  xy(z) coordinates. Default is `c(0, 0, 0)`

- geom:

  character. Named vector of colnames of x, y, (z) coordinate columns.
  Default is `c("sdimx", "sdimy", "sdimz")`

## Value

spatial locations

## Details

`scale_factor` either given as a single value where it will be applied
to x, y, and z (if available) dimensions or as a vector of named values
for 'x', y', (and 'z').
