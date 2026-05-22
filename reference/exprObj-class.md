# S4 exprObj

Framework to store aggregated expression information

## Value

exprObj

## Slots

- `name`:

  name of exprObj

- `exprMat`:

  matrix of expression information

- `spat_unit`:

  spatial unit of expression (e.g. 'cell')

- `feat_type`:

  feature type of expression (e.g. 'rna', 'protein')

- `provenance`:

  origin data of expression information (if applicable)

- `misc`:

  misc

## Examples

``` r
GiottoData::loadSubObjectMini("exprObj")
#> An object of class exprObj : "normalized"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 337 x 462 sparse Matrix of class "dgCMatrix"
#>                                                                                          
#> Mlc1    .       .        .        .        . .  5.291601 . .        5.821597 . .        .
#> Gprc5b  .       6.398031 .        7.454658 . .  6.273066 . 7.680054 6.808785 . 5.082989 .
#> Gfap   10.48137 .        7.901442 5.886051 . . 10.746982 . 5.701006 5.821597 . .        .
#>              
#> Mlc1   ......
#> Gprc5b ......
#> Gfap   ......
#> 
#>  ........suppressing 449 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
```
