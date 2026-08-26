# convert_to_full_spatial_network

Convert to a full spatial network, ie ensuring that all edges that may
currently only be represented as \\a\\ -\> \\b\\ also have the reverse
\\b\\ -\> \\a\\. The entries are then made unique, after which all
interactions are ranked by distance, where rank increases from smaller
to larger distances. This rank is appended to the `data.table` as a
`rank_int` column. Another `rnk_src_trgt` column is added with the IDs
of \\a\\ and \\b\\ pasted together

## Usage

``` r
convert_to_full_spatial_network(reduced_spatial_network_DT)
```

## Arguments

- reduced_spatial_network_DT:

  reduced spatial network in `data.table` format

## Value

data.table

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

spat_net_full <- convert_to_full_spatial_network(spat_net)
```
