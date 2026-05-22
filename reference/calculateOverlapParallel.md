# calculateOverlapParallel

calculate overlap between cellular structures (polygons) and features
(points)

## Usage

``` r
calculateOverlapParallel(
  gobject,
  name_overlap = NULL,
  spatial_info = "cell",
  feat_info = "rna",
  poly_ID_names = "all",
  polygon_group_size = 500,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- name_overlap:

  name for the overlap results (default to feat_info parameter)

- spatial_info:

  polygon information

- feat_info:

  feature information

- poly_ID_names:

  list of poly_IDs to use

- polygon_group_size:

  number of polygons to process per parallelization group

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

## Value

giotto object or spatVector with overlapping information

## Details

parallel follows the future approach. This means that plan(multisession)
does not work, since the underlying terra objects are internal C
pointers. plan(multicore) is also not supported for Rstudio users.

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

calculateOverlapParallel(g, spatial_info = "z1")
#> Warning: Your code is running sequentially. For better performance, consider using a
#>  parallel plan like:
#>  future::plan(future::multisession)
#> 
#>  To silence this warning, set options("giotto.warn_sequential" = FALSE)
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
