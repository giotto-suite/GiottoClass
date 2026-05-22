# createNearestNetwork

create a nearest neighbour (NN) network

## Usage

``` r
createNearestNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  type = c("sNN", "kNN"),
  dim_reduction_to_use = "pca",
  dim_reduction_name = NULL,
  dimensions_to_use = seq_len(10),
  feats_to_use = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  name = NULL,
  return_gobject = TRUE,
  k = 30,
  minimum_shared = 5,
  top_shared = 3,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- type:

  sNN or kNN

- dim_reduction_to_use:

  dimension reduction method to use

- dim_reduction_name:

  name of dimension reduction set to use

- dimensions_to_use:

  number of dimensions to use as input

- feats_to_use:

  if dim_reduction_to_use = NULL, which genes to use

- expression_values:

  expression values to use

- name:

  arbitrary name for NN network. Defaults to
  \[type\].\[dim_reduction_to_use\]

- return_gobject:

  boolean: return giotto object (default = TRUE)

- k:

  number of k neighbors to use

- minimum_shared:

  minimum shared neighbors

- top_shared:

  keep at ...

- verbose:

  be verbose

- ...:

  additional parameters for kNN and sNN functions from dbscan

## Value

giotto object with updated NN network

## Details

This function creates a k-nearest neighbour (kNN) or shared nearest
neighbour (sNN) network based on the provided dimension reduction space.
To run it directly on the gene expression matrix set
*dim_reduction_to_use = NULL*.

See also [`kNN`](https://rdrr.io/pkg/dbscan/man/kNN.html) and
[`sNN`](https://rdrr.io/pkg/dbscan/man/sNN.html) for more information
about how the networks are created.

Output for kNN:

- **from:** cell_ID for source cell

- **to:** cell_ID for target cell

- **distance:** distance between cells

- **weight:** \\1/(1 + distance)\\

Output for sNN:

- **from:** cell_ID for source cell

- **to:** cell_ID for target cell

- **distance:** distance between cells

- **weight:** \\1/(1 + distance)\\

- **shared:** number of shared neighbours

- **rank:** ranking of pairwise cell neighbours

For sNN networks two additional parameters can be set:

- **minimum_shared:** minimum number of shared neighbours needed

- **top_shared:** keep this number of the top shared neighbours,
  irrespective of minimum_shared setting

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

createNearestNetwork(g)
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 634, 624 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : cell 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [cell][rna] raw normalized scaled
#> spatial locations ----------------
#>   [cell] raw
#> spatial networks -----------------
#>   [cell] Delaunay_network spatial_network
#> spatial enrichments --------------
#>   [cell][rna] cluster_metagene DWLS
#> dim reduction --------------------
#>   [cell][rna] pca custom_pca umap custom_umap tsne
#> nearest neighbor networks --------
#>   [cell][rna] sNN.pca custom_NN
#> attached images ------------------
#> images      : alignment image 
#> 
#> 
#> Use objHistory() to see steps and params used
```
