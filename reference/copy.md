# Copy an entire object

S4 generic for Giotto's S4 subobjects to return with full copies of
certain subobjects that usually return referenced information.

## Usage

``` r
# S4 method for class 'coordDataDT'
copy(x)

# S4 method for class 'giottoPoints'
copy(x)

# S4 method for class 'giottoPolygon'
copy(x)

# S4 method for class 'giottoLargeImage'
copy(x)
```

## Arguments

- x:

  a Giotto S4 class subobject

## Value

giotto subobjects

## See also

[`copy`](https://rdrr.io/pkg/data.table/man/copy.html)
[`deepcopy`](https://rspatial.github.io/terra/reference/deepcopy.html)

## Examples

``` r
g <- GiottoData::loadSubObjectMini("exprObj")

copy(g)
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
