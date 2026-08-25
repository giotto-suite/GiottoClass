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

list_cell_id_names(g)
```
