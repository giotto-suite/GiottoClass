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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

list_feature_info_names(g)
#> [1] "rna"
```
