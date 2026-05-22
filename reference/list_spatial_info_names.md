# list_spatial_info_names

return the available names for giotto spatial polygon information

## Usage

``` r
list_spatial_info_names(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

vector with names of available spatial polygon information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

list_spatial_info_names(g)
#> [1] "z0"        "z1"        "aggregate"
```
