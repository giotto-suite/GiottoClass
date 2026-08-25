# list_spatial_networks_names

return the available names for giotto feature information

## Usage

``` r
list_spatial_networks_names(gobject, spat_unit = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

## Value

vector with names of available feature information

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

list_spatial_networks_names(g, spat_unit = "cell")
```
