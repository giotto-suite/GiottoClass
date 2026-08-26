# Get nearest neighbor network

Get a NN-network from a Giotto object

## Usage

``` r
getNearestNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  nn_type = NULL,
  name = NULL,
  output = c("nnNetObj", "igraph", "data.table"),
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

- nn_type:

  "kNN" or "sNN"

- name:

  name of NN network to be used

- output:

  return a giotto `nnNetObj`, `igraph`, `data.table` object. Default
  'nnNetObj'

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

Giotto `nnNetObj`, `igraph` or `data.table` object

## See also

Other expression space nearest network accessor functions:
[`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/setNearestNetwork.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
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

getNearestNetwork(gobject = g)
#> The NN network type was not specified, default to the
#>  first: "sNN"
#> The NN network name was not specified, default to the
#>  first: "sNN.pca"
#> An object of class nnNetObj : "sNN.pca"
#> --| Contains nearest neighbor network generated with: sNN 
#> ----| for feat_type: rna 
#> ----|     spat_unit: cell 
#> ----|     provenance: cell 
#> 
#> IGRAPH 999a381 DNW- 624 3748 -- 
#> + attr: name (v/c), weight (e/n), distance (e/n), shared (e/n), rank
#> | (e/n)
#> + edges from 999a381 (vertex names):
#>  [1] AAAGGGATGTAGCAAG-1->CAGTGTCCGCAGAATG-1
#>  [2] AAAGGGATGTAGCAAG-1->ACGTAGATTGCTGATG-1
#>  [3] AAAGGGATGTAGCAAG-1->CGTACCTGATAGGCCT-1
#>  [4] AAAGGGATGTAGCAAG-1->GATAAATCGGTGGATG-1
#>  [5] AAAGGGATGTAGCAAG-1->AACCCAGAGACGGAGA-1
#>  [6] AAAGGGATGTAGCAAG-1->GCCTTCAGCCCTACCG-1
#>  [7] AAATGGCATGTCTTGT-1->ACCCTTCATCTGCGAA-1
#> + ... omitted several edges
#> 
#> 
```
