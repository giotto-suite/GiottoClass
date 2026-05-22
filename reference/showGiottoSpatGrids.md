# showGiottoSpatGrids

Prints the available spatial grids that are attached to the Giotto
object

## Usage

``` r
showGiottoSpatGrids(gobject, nrows = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print

## Value

prints name of available spatial grids

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
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatLocs.md),
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
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)

showGiottoSpatGrids(g)
#> └──Spatial unit "cell"
#>    └──Feature type "rna"
#>       └──S4 spatialGridObj "spatial_grid"   (388800 rows)
#>                x_start y_start  x_end y_end gr_name gr_x_name gr_y_name
#>                  <num>   <num>  <num> <num>  <char>    <char>    <char>
#>             1:  3067.5   -5445 3072.5 -5440    gr_1    gr_x_1    gr_y_1
#>             2:  3072.5   -5445 3077.5 -5440    gr_2    gr_x_2    gr_y_1
#>             3:  3077.5   -5445 3082.5 -5440    gr_3    gr_x_3    gr_y_1
#>             4:  3082.5   -5445 3087.5 -5440    gr_4    gr_x_4    gr_y_1
#>          
```
