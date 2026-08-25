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
  engine = c("dbscan", "hnsw"),
  ef = 200,
  n_threads_build = 1L,
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

- engine:

  character. kNN search backend. `"dbscan"` (default) is exact and
  single-threaded, and is the better choice on small data where it is
  also faster. `"hnsw"` uses an approximate HNSW index via
  [`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
  – multithreaded and free of the kd-tree's degradation at PCA
  dimensionality, which is what makes it worth reaching for on large
  datasets. On 158,662 cells at `k = 30` it reproduced 99.995% of the
  exact network's undirected edges in 11.8s against 79.9s. Requires
  RcppHNSW. An `"auto"` setting that picks by dataset size is planned.

- ef:

  integer. `"hnsw"` only, ignored otherwise. Search beam width, default
  200 – the recall/speed dial. Higher values search more of the graph,
  moving the result closer to the exact `"dbscan"` answer at the cost of
  query time.

- n_threads_build:

  integer or `NULL`. `"hnsw"` only, ignored otherwise. Threads for the
  index build, default `1`. A parallel build is not reproducible –
  insertion order varies, so neighbours differ slightly between runs and
  that propagates to clustering even with a fixed seed. `NULL` inherits
  the search thread count and trades reproducibility for speed.

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

createNearestNetwork(g)
```
