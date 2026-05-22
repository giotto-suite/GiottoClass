# get_distance

estimate average distance between neighboring cells with network table
as input

## Usage

``` r
get_distance(networkDT, method = c("mean", "median"))
```

## Arguments

- networkDT:

  networkDT

- method:

  method

## Value

numeric

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
spat_net <- getSpatialNetwork(g, output = "networkDT")

get_distance(spat_net, method = "mean")
#> [1] 137.93
```
