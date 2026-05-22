# showGiottoSpatLocs

shows the available spatial locations

## Usage

``` r
showGiottoSpatLocs(gobject, nrows = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print for each spatial location data.table

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
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md),
[`showGiottoSpatNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatNetworks.md),
[`showGiottoSpatialInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatialInfo.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

showGiottoSpatLocs(g)
#> └──Spatial unit "cell"
#>    └──S4 spatLocsObj "raw" coordinates:   (624 rows)
#>          An object of class spatLocsObj 
#>          provenance: cell
#>              ------------------------
#>             sdimx sdimy            cell_ID
#>             <int> <int>             <char>
#>          1:  5477 -4125 AAAGGGATGTAGCAAG-1
#>          2:  5959 -2808 AAATGGCATGTCTTGT-1
#>          3:  4720 -5202 AAATGGTCAATGTGCC-1
#>          4:  5202 -5322 AAATTAACGGGTAGCT-1
#>          
#>          ranges:
#>               sdimx sdimy
#>          [1,]  3069 -5442
#>          [2,]  6441 -2568
#>          
#>          
#>       
```
