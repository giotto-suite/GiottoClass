# list_spatial_grids

return the available spatial grids that are attached to the Giotto
object

## Usage

``` r
list_spatial_grids(
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

data.table of names and locations of available spatial grids. col order
matters

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

list_spatial_grids(g)
#>    spat_unit feat_type         name
#>       <char>    <char>       <char>
#> 1:      cell       rna spatial_grid
```
