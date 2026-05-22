# Convert spatialNetworkObj to igraph

Convert a `spatialNetworkObj` to a non-directed igraph representation.

## Usage

``` r
spat_net_to_igraph(spatialNetworkObj, attr = NULL)
```

## Arguments

- spatialNetworkObj:

  spatialNetworkObj

- attr:

  columns to include as edge attributes.

## Value

igraph

## Examples

``` r
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
# only name attribute
g <- spat_net_to_igraph(sn)

# view other column info besides to and from cols
head(sn[], 1)
#>                                       from
#>                                     <char>
#> 1: 100210519278873141813371229408401071444
#>                                         to sdimx_begin sdimy_begin sdimx_end
#>                                     <char>       <num>       <num>     <num>
#> 1: 216701996099683814512199402223708471323    6637.881   -5140.465   6612.68
#>    sdimy_end distance     weight
#>        <num>    <num>      <num>
#> 1: -5145.746 25.74799 0.03883798

# include distance and weight col info
g <- spat_net_to_igraph(sn, attr = c("distance", "weight"))
```
