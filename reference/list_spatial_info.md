# list_spatial_info

return the available giotto spatial polygon information

## Usage

``` r
list_spatial_info(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

names of available spatial polygon information

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

list_spatial_info(g)
#>    spat_info
#>       <char>
#> 1:        z0
#> 2:        z1
#> 3: aggregate
```
