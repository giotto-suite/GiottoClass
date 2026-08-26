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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

list_feature_info(g)
#>    feat_info
#>       <char>
#> 1:       rna
```
