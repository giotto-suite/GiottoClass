# list_spatial_grids_names

return the available spatial grids name for a given spatial unit that
are attached to the Giotto object

## Usage

``` r
list_spatial_grids_names(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  return_uniques = FALSE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

## Value

vector with names of available spatial grids names

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)

list_spatial_grids_names(g, spat_unit = "cell", feat_type = "rna")
#> [1] "spatial_grid"
```
