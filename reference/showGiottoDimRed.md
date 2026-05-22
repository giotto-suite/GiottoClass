# showGiottoDimRed

shows the available dimension reductions

## Usage

``` r
showGiottoDimRed(gobject, nrows = 3, ncols = 2)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of coordinates rows to print

- ncols:

  number of coordinates columns to print

## Value

prints the name and small subset of available dimension reduction
coordinates

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatInfo.md),
[`showGiottoFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatMetadata.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoNearestNetworks.md),
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
#>  active environment : 'giotto_env'
#>  python version : 3.10

showGiottoDimRed(g)
#> Dim reduction on cells:
#>  ------------------------- 
#> 
#> .
#> └──Spatial unit "cell"
#>    └──Feature type "rna"
#>       ├──Dim reduction type "pca"
#>       │  ├──S4 dimObj "pca" coordinates:   (624 rows 100 cols)
#>       │  │                            Dim.1      Dim.2
#>       │  │     AAAGGGATGTAGCAAG-1  0.801127   2.874369
#>       │  │     AAATGGCATGTCTTGT-1  6.995542  -2.010611
#>       │  │     AAATGGTCAATGTGCC-1 -7.813633 -12.305266
#>       │  │  
#>       │  └──S4 dimObj "custom_pca" coordinates:   (624 rows 100 cols)
#>       │                                Dim.1     Dim.2
#>       │        AAAGGGATGTAGCAAG-1 -0.4547083  3.383001
#>       │        AAATGGCATGTCTTGT-1 -6.2679882  1.503456
#>       │        AAATGGTCAATGTGCC-1 10.2690789 -7.006071
#>       │     
#>       ├──Dim reduction type "umap"
#>       │  ├──S4 dimObj "umap" coordinates:   (624 rows 2 cols)
#>       │  │                             Dim.1      Dim.2
#>       │  │     AAAGGGATGTAGCAAG-1  -3.975629   4.401250
#>       │  │     AAATGGCATGTCTTGT-1 -16.788569 -20.950487
#>       │  │     AAATGGTCAATGTGCC-1  27.206731  -9.034316
#>       │  │  
#>       │  └──S4 dimObj "custom_umap" coordinates:   (624 rows 2 cols)
#>       │                                Dim.1      Dim.2
#>       │        AAAGGGATGTAGCAAG-1  -3.731145  -3.665592
#>       │        AAATGGCATGTCTTGT-1 -14.201382 -23.566582
#>       │        AAATGGTCAATGTGCC-1  29.736043   6.478635
#>       │     
#>       └──Dim reduction type "tsne"
#>          └──S4 dimObj "tsne" coordinates:   (624 rows 2 cols)
#>                                        Dim.1      Dim.2
#>                AAAGGGATGTAGCAAG-1 -3.4389554  -1.690378
#>                AAATGGCATGTCTTGT-1  0.4290782 -18.060330
#>                AAATGGTCAATGTGCC-1 17.8533998  12.975694
#>             
```
