# showGiottoExpression

shows the available matrices

## Usage

``` r
showGiottoExpression(gobject, nrows = 4, ncols = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print for each matrix (ignored for sparse matrices)

- ncols:

  number of columns to print for each matrix (ignored for sparse
  matrices)

## Value

prints the name and small subset of available matrices

## See also

Other functions to show data in giotto object:
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

showGiottoExpression(g)
#> └──Spatial unit "cell"
#>    └──Feature type "rna"
#>       ├──Expression data "raw" values:
#>       │     An object of class exprObj : "raw"
#>       │     spat_unit : "cell"
#>       │     feat_type : "rna"
#>       │     provenance: cell 
#>       │     
#>       │     contains:
#>       │     634 x 624 sparse Matrix of class "dgCMatrix"
#>       │                                             
#>       │     Gna12  1 2 1 1 9 1 3 5 3 . . 10 7 ......
#>       │     Ccnd2  . 1 1 . . 1 . 1 1 . .  . 3 ......
#>       │     Btbd17 . 1 1 1 . . 2 . . . .  . . ......
#>       │     
#>       │      ........suppressing 611 columns and 628 rows 
#>       │                                                   
#>       │     Gm19935       . 1 . . . . . . . . 1 . . ......
#>       │     9630013A20Rik . . . . . . . . . . 1 . . ......
#>       │     2900040C04Rik 1 . . . . . . . . 1 . . . ......
#>       │     
#>       │      First four colnames:
#>       │      AAAGGGATGTAGCAAG-1 AAATGGCATGTCTTGT-1
#>       │      AAATGGTCAATGTGCC-1 AAATTAACGGGTAGCT-1 
#>       │  
#>       ├──Expression data "normalized" values:
#>       │     An object of class exprObj : "normalized"
#>       │     spat_unit : "cell"
#>       │     feat_type : "rna"
#>       │     provenance: cell 
#>       │     
#>       │     contains:
#>       │     634 x 624 sparse Matrix of class "dgCMatrix"
#>       │                                                                                                
#>       │     Gna12  3.783033 3.929306 3.213481 3.625522 5.749157 2.399595 4.127923 4.693522 5.299374 . .
#>       │     Ccnd2  .        3.021024 3.213481 .        .        2.399595 .        2.578969 3.785883 . .
#>       │     Btbd17 .        3.021024 3.213481 3.625522 .        .        3.583641 .        .        . .
#>       │                                    
#>       │     Gna12  5.683074 5.036423 ......
#>       │     Ccnd2  .        3.871485 ......
#>       │     Btbd17 .        .        ......
#>       │     
#>       │      ........suppressing 611 columns and 628 rows 
#>       │                                                                               
#>       │     Gm19935       .        3.021024 . . . . . . . .        3.103487 . . ......
#>       │     9630013A20Rik .        .        . . . . . . . .        3.103487 . . ......
#>       │     2900040C04Rik 3.783033 .        . . . . . . . 3.488189 .        . . ......
#>       │     
#>       │      First four colnames:
#>       │      AAAGGGATGTAGCAAG-1 AAATGGCATGTCTTGT-1
#>       │      AAATGGTCAATGTGCC-1 AAATTAACGGGTAGCT-1 
#>       │  
#>       └──Expression data "scaled" values:
#>             An object of class exprObj 
#>             for spatial unit: "cell" and feature type: "rna"
#>               Provenance: cell
#>             
#>             contains:
#>             634 x 624 dense matrix of class "dgeMatrix"
#>             
#>             Gna12   0.3648960  0.3883749  0.02397458  0.2635371 ...... 
#>             Ccnd2  -0.9726302  0.5135323  0.59882469 -0.8883955 ...... 
#>             Btbd17 -0.6902939  1.0914525  1.12309944  1.3267401 ...... 
#>             Sox9   -0.8344595 -1.0154377 -0.84282003 -0.7614102 ...... 
#>             
#>              First four colnames:
#>              AAAGGGATGTAGCAAG-1 AAATGGCATGTCTTGT-1
#>              AAATGGTCAATGTGCC-1 AAATTAACGGGTAGCT-1 
#>          
```
