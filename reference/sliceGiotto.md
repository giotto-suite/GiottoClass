# Slice `giotto` object by `spat_unit` and `feat_type`

Extract specific spatial units and feature types from a `giotto` object
as independent `giotto` objects.

## Usage

``` r
sliceGiotto(
  gobject,
  spat_unit = ":all:",
  feat_type = ":all:",
  negate = FALSE,
  verbose = FALSE
)
```

## Arguments

- gobject:

  `giotto` object

- spat_unit:

  character vector. Spatial units to slice out. ":all:" means keeping
  all of them in the output

- feat_type:

  character vector. Feature types to slice out. ":all:" means keeping
  all of them in the output

- negate:

  logical. If `TRUE`, all specified `spat_unit` and `feat_type` are
  **not** kept. `":all:"` tokens are ignored.

- verbose:

  be verbose

## Value

`giotto` object

## See also

[`subsetGiotto()`](https://giotto-suite.github.io/GiottoClass/reference/subsetGiotto.md)
[subset_giotto](https://giotto-suite.github.io/GiottoClass/reference/subset_giotto.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
res <- sliceGiotto(g, spat_unit = "aggregate")
force(res)
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 462 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
```
