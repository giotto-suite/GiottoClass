# Create a Network

Generic for constructing a network (graph) from coordinates, features,
or an embedding. Methods dispatch on the input data class and a
[networkParam](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam-class.md)-inheriting
object that selects the algorithm (kNN, sNN, Delaunay, ...). Part of the
`create<Noun>` object-construction family. Distinct from analysis-stage
operations such as
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md),
[`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md),
[`reduceData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceData.md),
and
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md).

## Usage

``` r
createNetwork(x, param, ...)

# S4 method for class 'matrix,kNNNetworkParam'
createNetwork(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...)

# S4 method for class 'matrix,sNNNetworkParam'
createNetwork(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...)

# S4 method for class 'matrix,delaunayNetworkParam'
createNetwork(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...)

# S4 method for class 'matrix,missing'
createNetwork(
  x,
  param,
  type = c("sNN", "kNN", "delaunay"),
  method = c("dbscan", "geometry", "RTriangle", "deldir"),
  node_ids = NULL,
  include_distance = TRUE,
  include_weight = TRUE,
  as.igraph = TRUE,
  verbose = NULL,
  backend = NULL,
  ...
)

# S4 method for class 'spatLocsObj,networkParam'
createNetwork(x, param, node_ids = NULL, ...)

# S4 method for class 'dimObj,networkParam'
createNetwork(x, param, dimensions_to_use = NULL, ...)

# S4 method for class 'giotto,NNNetworkParam'
createNetwork(
  x,
  param,
  spat_unit = NULL,
  feat_type = NULL,
  space = c("expression", "spatial"),
  dim_reduction_to_use = "pca",
  dim_reduction_name = NULL,
  dimensions_to_use = seq_len(10L),
  spat_loc_name = "raw",
  ...
)

# S4 method for class 'giotto,delaunayNetworkParam'
createNetwork(x, param, spat_unit = NULL, spat_loc_name = "raw", ...)
```

## Arguments

- x:

  a data object (matrix,
  [spatLocsObj](https://giotto-suite.github.io/GiottoClass/dev/reference/spatLocsObj-class.md),
  [dimObj](https://giotto-suite.github.io/GiottoClass/dev/reference/dimObj-class.md),
  [giotto](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md),
  or a GiottoDisk `fileStore`)

- param:

  a
  [networkParam](https://giotto-suite.github.io/GiottoClass/dev/reference/networkParam-class.md)-inheriting
  object

- ...:

  additional arguments, for use in specific methods

- dimensions_to_use:

  integer vector; columns of the `dimObj` matrix to keep when building
  the network. `NULL` (default) keeps all.

- spat_unit:

  spatial unit (`giotto` method)

- feat_type:

  feature type (`giotto` method, NN networks)

- space:

  for NN networks on a `giotto` object: which space the neighborhood is
  defined in. Default `"expression"` (pulls a dimension reduction such
  as PCA). Set to `"spatial"` to build a spatial kNN/sNN from cell
  coordinates.

- dim_reduction_to_use:

  name of the reduction family to pull from the `giotto` object (default
  `"pca"`). Only used when `space = "expression"`.

- dim_reduction_name:

  specific reduction name. Only used when `space = "expression"`.

- spat_loc_name:

  spatial-locations name. Used by the Delaunay method and by NN methods
  when `space = "spatial"`.

## Value

A network. Concrete type depends on the Param's `output` slot and any
supplied `backend`: `"data.table"` of edges, `igraph`, or a GiottoDisk
`parquetEdgeStore`.
