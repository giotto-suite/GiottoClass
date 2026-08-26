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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
spat_net <- getSpatialNetwork(g, output = "networkDT")

get_distance(spat_net, method = "mean")
#> [1] 137.93
```
