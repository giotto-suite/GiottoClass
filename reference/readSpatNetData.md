# Read spatial networks

read spatial networks data from list

## Usage

``` r
readSpatNetData(
  data_list,
  default_spat_unit = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of spatial network input data

- default_spat_unit:

  (optional) default spat_unit to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

spatialNetworkObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatialNetworkObj", idx = 2)

readSpatNetData(x)
#> [[1]]
#> An object of class spatialNetworkObj : "kNN_network"
#> Contains spatial network generated with: dbscan 
#> spat_unit : "aggregate"
#> provenance: z0 z1 
#>    2211 connections (filtered)
#> 
```
