# annotate_spatlocs_with_spatgrid_3D

annotate spatial locations with 3D spatial grid information

## Usage

``` r
annotate_spatlocs_with_spatgrid_3D(spatloc, spatgrid)
```

## Arguments

- spatloc:

  spatial_locs slot from giotto object

- spatgrid:

  selected spatial_grid slot from giotto object

## Value

annotated spatial location data.table

## Examples

``` r
g <- GiottoData::loadGiottoMini("starmap")
g_spatloc <- getSpatialLocations(g, output = "data.table")
g_spatgrid <- getSpatialGrid(g)

annotate_spatlocs_with_spatgrid_3D(
    spatloc = g_spatloc,
    spatgrid = g_spatgrid
)
```
