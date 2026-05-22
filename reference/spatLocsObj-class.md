# S4 spatLocsObj Class

Framework to store spatial location information

## Value

spatLocsObj

## Slots

- `name`:

  name of spatLocsObj

- `coordinates`:

  data.table of spatial coordinates/locations

- `spat_unit`:

  spatial unit tag

- `provenance`:

  origin of aggregated information (if applicable)

## Examples

``` r
GiottoData::loadSubObjectMini("spatLocsObj")
#> An object of class spatLocsObj : "raw"
#> spat_unit : "aggregate"
#> provenance: z0 z1 
#> dimensions: 462 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6637.881 -5140.465 100210519278873141813371229408401071444
#> 2: 6471.978 -4883.541 101161259912191124732236989250178928032
#> 3: 6801.610 -4968.685 101488859781016188084173008420811094152
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6401.412 -5146.747
#> [2,] 6899.108 -4700.326
#> 
```
