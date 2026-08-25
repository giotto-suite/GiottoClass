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
spat_net <- getSpatialNetwork(g, output = "networkDT")

get_distance(spat_net, method = "mean")
```
