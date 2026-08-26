# list_spatial_locations

shows the available spatial locations

## Usage

``` r
list_spatial_locations(gobject, spat_unit = NULL, return_uniques = FALSE)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

## Value

names and locations of available data.table as data.table

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

list_spatial_locations(g)
#>    spat_unit   name
#>       <char> <char>
#> 1:      cell    raw
```
