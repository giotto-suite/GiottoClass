# showGiottoNearestNetworks

shows the available nearest neighbor networks

## Usage

``` r
showGiottoNearestNetworks(gobject, nrows = 3)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of network rows to print

## Value

prints the name and small subset of available nearest neighbor network
info

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatEnrichments.md),
[`showGiottoSpatGrids()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md),
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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

showGiottoNearestNetworks(g)
#> └──Spatial unit "cell"
#>    └──Feature type "rna"
#>       └──NN network type "sNN"
#>          ├──S4 nnNetObj "sNN.pca"   (3748 rows)
#>          │                      from                 to    weight distance shared  rank
#>          │                    <char>             <char>     <num>    <num>  <int> <int>
#>          │     1: AAAGGGATGTAGCAAG-1 CAGTGTCCGCAGAATG-1 0.3777898 1.646975      9     1
#>          │     2: AAAGGGATGTAGCAAG-1 ACGTAGATTGCTGATG-1 0.2318818 3.312541      6     2
#>          │     3: AAAGGGATGTAGCAAG-1 CGTACCTGATAGGCCT-1 0.3122768 2.202287      6     3
#>          │  
#>          └──S4 nnNetObj "custom_NN"   (1872 rows)
#>                                 from                 to    weight distance shared  rank
#>                               <char>             <char>     <num>    <num>  <int> <int>
#>                1: AAAGGGATGTAGCAAG-1 AACTGGGTCCCGACGT-1 0.1492788 5.698874      2     1
#>                2: AAAGGGATGTAGCAAG-1 CAGCTCGTGCTTGTGT-1 0.1507539 5.633328      1     2
#>                3: AAAGGGATGTAGCAAG-1 CGTACCTGATAGGCCT-1 0.1566434 5.383926      1     3
#>             
```
