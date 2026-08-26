# list_spatial_locations_names

lists the available spatial location names for a given spatial unit

## Usage

``` r
list_spatial_locations_names(gobject, spat_unit = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

## Value

vector with names of available spatial locations

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

list_spatial_locations_names(g, spat_unit = "cell")
#> [1] "raw"
```
