# List feat ID names

lists the available feat id names In effect, these names are the
feat_types and feature info in the gobject

## Usage

``` r
list_feat_id_names(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

vector with names of available sets of feat_IDs

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

list_feat_id_names(g)
```
