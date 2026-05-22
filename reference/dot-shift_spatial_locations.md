# Shift spatial locations

Shift given coordinates by given translation values

## Usage

``` r
.shift_spatial_locations(
  spatlocs,
  dx = 0,
  dy = 0,
  dz = 0,
  geom = c("sdimx", "sdimy", "sdimz"),
  copy_obj = TRUE
)
```

## Arguments

- spatlocs:

  spatial locations to use

- dx:

  value to shift coordinates in the positive x direction

- dy:

  value to shift coordinates in the positive y direction

- dz:

  value to shift coordinates in the positive z direction

- geom:

  character vector. Expected colnames of x, y, z coords

- copy_obj:

  logical. copy/duplicate object (default = TRUE)

## Value

spatial locations
