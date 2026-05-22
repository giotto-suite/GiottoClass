# Create kNN spatial feature network

Calculates the centroid locations for the giotto polygons

## Usage

``` r
createSpatialFeaturesKNNnetwork(
  gobject,
  method = "dbscan",
  feat_type = NULL,
  name = "knn_feats_network",
  k = 4,
  maximum_distance = NULL,
  minimum_k = 0,
  add_feat_ids = FALSE,
  verbose = TRUE,
  return_gobject = TRUE,
  toplevel_params = 2,
  ...
)
```

## Arguments

- gobject:

  giotto object

- method:

  kNN algorithm method

- feat_type:

  feature type to build feature network

- name:

  name of network

- k:

  number of neighbors

- maximum_distance:

  maximum distance bewteen features

- minimum_k:

  minimum number of neighbors to find

- add_feat_ids:

  add feature id names (default = FALSE, increases object size)

- verbose:

  be verbose

- return_gobject:

  return giotto object (default: TRUE)

- toplevel_params:

  toplevel value to pass when updating giotto params

- ...:

  additional parameters to pass to
  [`kNN`](https://rdrr.io/pkg/dbscan/man/kNN.html)

## Value

If `return_gobject = TRUE` a giotto object containing the network will
be returned. If `return_gobject = FALSE` the network will be returned as
a datatable.

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

createSpatialFeaturesKNNnetwork(g)
#> Convert feature spatial info to matrix
#> Create kNN network with dbscan
#> Filter output for distance and minimum neighbours
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
