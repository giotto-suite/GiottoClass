# Get dimension reduction

Function to get a dimension reduction object

## Usage

``` r
getDimReduction(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  reduction = c("cells", "feats"),
  reduction_method = NULL,
  name = NULL,
  output = c("dimObj", "matrix"),
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- reduction:

  reduction on cells or features (e.g. "cells", "feats")

- reduction_method:

  reduction method (e.g. "pca", "umap", "tsne")

- name:

  name of reduction results

- output:

  object type to return as. Either 'dimObj' (default) or 'matrix' of the
  embedding coordinates.

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

dim reduction object (default) or dim reduction coordinates

## See also

Other dimensional reduction data accessor functions:
[`setDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/setDimReduction.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
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

getDimReduction(g)
#> An object of class dimObj : "pca"
#> --| Contains dimension reduction generated with: pca 
#> ----| for feat_type: rna 
#> ----|     spat_unit: cell 
#> 
#>    100 dimensions for 624 data points
#> 
#> Additional included info:
#> [1] "eigenvalues" "loadings"   
#> 
```
