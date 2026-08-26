# showGiottoFeatMetadata

shows the available feature metadata

## Usage

``` r
showGiottoFeatMetadata(gobject, nrows = 4)
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
[`showGiottoCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoCellMetadata.md),
[`showGiottoDimRed()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoDimRed.md),
[`showGiottoExpression()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoExpression.md),
[`showGiottoFeatInfo()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoFeatInfo.md),
[`showGiottoImageNames()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoImageNames.md),
[`showGiottoNearestNetworks()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoNearestNetworks.md),
[`showGiottoSpatEnrichments()`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatEnrichments.md),
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

showGiottoFeatMetadata(g)
#> ├──Spatial unit "z0"
#> │  └──Feature type "rna"
#> │        An object of class featMetaObj 
#> │        spat_unit : "z0"
#> │        feat_type : "rna"
#> │        provenance: z0 
#> │        
#> │           feat_ID
#> │            <char>
#> │        1:    Mlc1
#> │        2:  Gprc5b
#> │        3:    Gfap
#> │     
#> ├──Spatial unit "z1"
#> │  └──Feature type "rna"
#> │        An object of class featMetaObj 
#> │        spat_unit : "z1"
#> │        feat_type : "rna"
#> │        provenance: z1 
#> │        
#> │           feat_ID
#> │            <char>
#> │        1:    Mlc1
#> │        2:  Gprc5b
#> │        3:    Gfap
#> │     
#> └──Spatial unit "aggregate"
#>    └──Feature type "rna"
#>          An object of class featMetaObj 
#>          spat_unit : "aggregate"
#>          feat_type : "rna"
#>          provenance: z0 z1 
#>          
#>             feat_ID nr_cells perc_cells total_expr mean_expr mean_expr_det hvg_orig
#>              <char>    <int>      <num>      <num>     <num>         <num>   <char>
#>          1:    Mlc1       80   17.31602   526.5915  1.139808      6.582394       no
#>          2:  Gprc5b      138   29.87013   944.0153  2.043323      6.840690       no
#>          3:    Gfap      151   32.68398  1229.1943  2.660594      8.140359      yes
#>                   var    hvf
#>                 <num> <char>
#>          1:  1.815718    yes
#>          2:  2.388911    yes
#>          3: 12.899599    yes
#>       
```
