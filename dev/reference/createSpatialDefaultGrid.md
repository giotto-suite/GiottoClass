# createSpatialDefaultGrid

Create a spatial grid using the default method

## Usage

``` r
createSpatialDefaultGrid(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  sdimx_stepsize = NULL,
  sdimy_stepsize = NULL,
  sdimz_stepsize = NULL,
  minimum_padding = 1,
  name = NULL,
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  spatial location name

- sdimx_stepsize:

  stepsize along the x-axis

- sdimy_stepsize:

  stepsize along the y-axis

- sdimz_stepsize:

  stepsize along the z-axis

- minimum_padding:

  minimum padding on the edges

- name:

  name for spatial grid (default = 'spatial_grid')

- return_gobject:

  boolean: return giotto object (default = TRUE)

## Value

giotto object with updated spatial grid slot

## Details

Creates a spatial grid with defined x, y (and z) dimensions. The
dimension units are based on the provided spatial location units.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

createSpatialDefaultGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
```
