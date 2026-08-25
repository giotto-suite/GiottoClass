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

list_spatial_locations_names(g, spat_unit = "cell")
```
