# List cell ID names

lists the available cell id names. In effect, these names are the
spat_units and poly info in the gobject

## Usage

``` r
list_cell_id_names(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

vector with names of available sets of cell_IDs

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

list_cell_id_names(g)
#> [1] "cell"
```
