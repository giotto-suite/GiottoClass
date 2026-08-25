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

list_spatial_locations(g)
```
