# list_feature_info

return the available giotto spatial feature information

## Usage

``` r
list_feature_info(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

names of available feature information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

list_feature_info(g)
```
