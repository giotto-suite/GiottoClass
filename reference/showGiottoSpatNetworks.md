# showGiottoSpatNetworks

Prints the available spatial networks that are attached to the Giotto
object

## Usage

``` r
showGiottoSpatNetworks(gobject, nrows = 4)
```

## Arguments

- gobject:

  a giotto object

- nrows:

  number of rows to print

## Value

prints names and small subset of available spatial network info

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
[`showGiottoSpatLocs()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatLocs.md),
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

showGiottoSpatNetworks(g)
#> └──Spatial unit "cell"
#>    ├──S4 spatialNetworkObj "Delaunay_network"   (1770 rows)
#>    │                      from                 to sdimx_begin sdimy_begin sdimx_end
#>    │                    <char>             <char>       <num>       <num>     <num>
#>    │     1: AAAGGGATGTAGCAAG-1 TCAAACAACCGCGTCG-1        5477       -4125      5340
#>    │     2: AAAGGGATGTAGCAAG-1 ACGATCATACATAGAG-1        5477       -4125      5546
#>    │     3: AAAGGGATGTAGCAAG-1 TATGCTCCCTACTTAC-1        5477       -4125      5408
#>    │     4: AAAGGGATGTAGCAAG-1 TTGTTCAGTGTGCTAC-1        5477       -4125      5615
#>    │        sdimy_end distance      weight
#>    │            <num>    <num>       <num>
#>    │     1:     -4125 137.0000 0.007299270
#>    │     2:     -4244 137.5573 0.007269700
#>    │     3:     -4244 137.5573 0.007269700
#>    │     4:     -4125 138.0000 0.007246377
#>    │  
#>    └──S4 spatialNetworkObj "spatial_network"   (3288 rows)
#>                           from                 to sdimx_begin sdimy_begin sdimx_end
#>                         <char>             <char>       <int>       <int>     <int>
#>          1: AAAGGGATGTAGCAAG-1 TCAAACAACCGCGTCG-1        5477       -4125      5340
#>          2: AAAGGGATGTAGCAAG-1 ACGATCATACATAGAG-1        5477       -4125      5546
#>          3: AAAGGGATGTAGCAAG-1 TATGCTCCCTACTTAC-1        5477       -4125      5408
#>          4: AAAGGGATGTAGCAAG-1 TTGTTCAGTGTGCTAC-1        5477       -4125      5615
#>             sdimy_end distance      weight
#>                 <int>    <num>       <num>
#>          1:     -4125 137.0000 0.007246377
#>          2:     -4244 137.5573 0.007217233
#>          3:     -4244 137.5573 0.007217233
#>          4:     -4125 138.0000 0.007194245
#>       
```
