# Get expression values

Function to get expression values from giotto object

## Usage

``` r
getExpression(
  gobject,
  values = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  output = c("exprObj", "matrix"),
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- values:

  expression values to extract (e.g. "raw", "normalized", "scaled")

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- output:

  what object type to retrieve the expression as. Currently either
  matrix' for the matrix object contained in the exprObj or 'exprObj'
  (default) for the exprObj itself are allowed.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

exprObj or matrix depending on output param

## See also

Other expression accessor functions:
[`setExpression()`](https://giotto-suite.github.io/GiottoClass/reference/setExpression.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialGrid.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

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

getExpression(g)
#> An object of class exprObj : "raw"
#> spat_unit : "cell"
#> feat_type : "rna"
#> provenance: cell 
#> 
#> contains:
#> 634 x 624 sparse Matrix of class "dgCMatrix"
#>                                         
#> Gna12  1 2 1 1 9 1 3 5 3 . . 10 7 ......
#> Ccnd2  . 1 1 . . 1 . 1 1 . .  . 3 ......
#> Btbd17 . 1 1 1 . . 2 . . . .  . . ......
#> 
#>  ........suppressing 611 columns and 628 rows 
#>                                               
#> Gm19935       . 1 . . . . . . . . 1 . . ......
#> 9630013A20Rik . . . . . . . . . . 1 . . ......
#> 2900040C04Rik 1 . . . . . . . . 1 . . . ......
#> 
#>  First four colnames:
#>  AAAGGGATGTAGCAAG-1 AAATGGCATGTCTTGT-1
#>  AAATGGTCAATGTGCC-1 AAATTAACGGGTAGCT-1 
```
