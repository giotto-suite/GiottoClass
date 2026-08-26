# showGiottoSpatEnrichments

shows the available spatial enrichment results

## Usage

``` r
showGiottoSpatEnrichments(gobject, nrows = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print for each spatial enrichment data.table

## Value

prints the name and small subset of available data.table

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md),
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatLocs.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatialInfo.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

showGiottoSpatEnrichments(g)
#> Spatial unit:  aggregate  
#> 
#> --> Feature type:  rna  
#> 
#> ----> Name  cluster_metagene : 
#>           1         2         3         4     5
#>       <num>     <num>     <num>     <num> <num>
#> 1: 1.014837 0.0000000 0.3160792 0.0000000     0
#> 2: 3.207415 0.9579716 0.6728505 0.2132677     0
#> 3: 3.953661 0.4604975 0.0000000 0.2302488     0
#> 4: 4.093622 0.5886051 0.1962017 0.5886051     0
#>                                    cell_ID
#>                                     <char>
#> 1: 240649020551054330404932383065726870513
#> 2: 274176126496863898679934791272921588227
#> 3: 323754550002953984063006506310071917306
#> 4:  87260224659312905497866017323180367450
```
