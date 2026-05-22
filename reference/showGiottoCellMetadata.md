# showGiottoCellMetadata

shows the available cell metadata

## Usage

``` r
showGiottoCellMetadata(gobject, nrows = 4)
```

## Arguments

- gobject:

  giotto object

- nrows:

  number of rows to print for each metadata

## Value

prints the name and small subset of available metadata

## See also

Other functions to show data in giotto object:
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
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

showGiottoCellMetadata(g)
#> └──Spatial unit "cell"
#>    └──Feature type "rna"
#>          An object of class cellMetaObj 
#>          spat_unit : "cell"
#>          feat_type : "rna"
#>          provenance: cell 
#>          dimensions: 4 7 
#>          
#>                        cell_ID in_tissue nr_feats perc_feats total_expr leiden_clus
#>                         <char>     <int>    <int>      <num>      <num>       <num>
#>          1: AACTCGATGGCGCAGT-1         1      265   41.79811  1057.9308           2
#>          2: GGCTGGCTAGCTTAAA-1         1      279   44.00631  1064.7493           5
#>          3: GACGCCTGTTGCAGGG-1         1      219   34.54259   964.9294           2
#>             custom_leiden
#>                     <num>
#>          1:             4
#>          2:             3
#>          3:             3
#>       
```
