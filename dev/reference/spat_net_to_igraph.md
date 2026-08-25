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

# include distance and weight col info
g <- spat_net_to_igraph(sn, attr = c("distance", "weight"))
```
