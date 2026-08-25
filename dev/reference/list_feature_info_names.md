# list_feature_info_names

return the available names for giotto feature information

## Usage

``` r
list_feature_info_names(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

vector with names of available feature information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

list_feature_info_names(g)
```
