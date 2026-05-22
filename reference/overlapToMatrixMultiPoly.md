# overlapToMatrixMultiPoly

create a count matrix based on overlap results from
[`calculateOverlapRaster`](https://giotto-suite.github.io/GiottoClass/reference/calculateOverlapRaster.md),
[`calculateOverlapSerial`](https://giotto-suite.github.io/GiottoClass/reference/calculateOverlapSerial.md),
or
[`calculateOverlapParallel`](https://giotto-suite.github.io/GiottoClass/reference/calculateOverlapParallel.md)
and aggregate information from multiple polygon layers (e.g. z-stacks)
together

## Usage

``` r
overlapToMatrixMultiPoly(
  gobject,
  name = "raw",
  poly_info = "cell",
  feat_info = "rna",
  new_poly_info = "multi",
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- name:

  name for the overlap count matrix

- poly_info:

  vector with polygon information

- feat_info:

  feature information

- new_poly_info:

  name for new aggregated polygon information

- return_gobject:

  return giotto object (default: TRUE)

## Value

giotto object or count matrix

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

overlapToMatrixMultiPoly(g, poly_info = "z0")
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#>   [multi][rna] raw
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
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
