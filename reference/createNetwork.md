# Create a network

Create networks from node values. This is a hub function for many
different methods of finding nearest neighbors. See details for
additional params important for generating specific types of networks.

## Usage

``` r
createNetwork(
  x,
  type = c("sNN", "kNN", "delaunay"),
  method = c("dbscan", "geometry", "RTriangle", "deldir"),
  node_ids = NULL,
  include_distance = TRUE,
  include_weight = TRUE,
  as.igraph = TRUE,
  verbose = NULL,
  ...
)
```

## Arguments

- x:

  matrix. Data to treat as nodes. Examples include expression
  information, PCA matrix, spatial locations xy(z) coordinates.

- type:

  type of network to create. Currently: "sNN", "kNN", or "delaunay"

- method:

  method used to create the type of network requested. One of "dbscan"
  for sNN and kNN or "geometry", "RTriangle", or "deldir" for delaunay.

- node_ids:

  character. Node ID values to assign. If NULL, rownames of input matrix
  will be used. If no rownames, integer indices will be used as node
  IDs.

- include_distance:

  logical. include edge distance attribute in output

- include_weight:

  logical. include edge weight attribute in output

- as.igraph:

  logical. Whether to return as `igraph`. Otherwise returns as
  `data.table`

- verbose:

  be verbose. Default = NULL (uses giotto verbosity option)

- ...:

  additional params to pass. See details section

## Value

Either `igraph` if `as.igraph = TRUE` and `data.table` otherwise.

## Details

Additional params are described below. Items in parenthesis refer to
which network types and/or methods the params are specific to.

- \[**`k`**\] numeric. (*sNN, kNN*) number of neighbors to find. Default
  is 30

- \[**`minimum_shared`**\] numeric. (*sNN*) minimum shared neighbors
  allowed per edge

- \[**`top_shared`**\] numeric. (*sNN*) keep at least this many edges
  per node, where kept edges are top ranked in terms of number of shared
  neighbors.

- \[**`filter`**\] logical. (*kNN*) whether to filter for only unique
  edges and apply `minimum_k` and `maximum_distance` filters. Should be
  set `TRUE` when generating a spatial kNN network. Default is `FALSE.`

- \[**`minimum_k`**\] (*delaunay, kNN*) minimum nearest neighbours if
  `maximum_distance != NULL`

- \[**`maximum_distance`**\] (*delaunay, kNN*) edge maximum euclidean
  distance allowed

- \[**`Y`**\] (*RTriangle*) If TRUE prohibits the insertion of Steiner
  points on the mesh boundary. Default is TRUE

- \[**`j`**\] (*RTriangle*) If TRUE jettisons vertices that are not part
  of the final triangulation from the output. Default is TRUE

- \[**`S`**\] (*RTriangle*) Specifies the maximum number of added
  Steiner points. Default is 0

- \[**`options`**\] (*geometry*) default is "Pp". See
  [geometry::delaunayn](https://rdrr.io/pkg/geometry/man/delaunayn.html)

- \[**`weight_fun`**\] function to calculate weights based on distance
  if `include_weight = TRUE`. Default is \\weight = 1 / (1 + distance)\\
  for `"kNN"` and `"sNN"` types and \\weight = 1 / distance\\ for
  `delaunay` type networks

## Examples

``` r
pca <- GiottoData::loadSubObjectMini("dimObj")[]
sl <- GiottoData::loadSubObjectMini("spatLocsObj")[]

# Delaunay via geometry::delaunayn()
del_geom <- createNetwork(
    x = as.matrix(sl[, .(sdimx, sdimy)]),
    type = "delaunay",
    method = "geometry",
    include_weight = TRUE,
    weight_fun = function(d) 1 / d,
    as.igraph = FALSE,
    node_ids = sl$cell_ID
)

# Delaunay via RTriangle::triangulate()
del_rt <- createNetwork(
    x = as.matrix(sl[, .(sdimx, sdimy)]),
    type = "delaunay",
    method = "RTriangle",
    include_weight = TRUE,
    weight_fun = function(d) 1 / d,
    as.igraph = FALSE,
    node_ids = sl$cell_ID
)

# Delaunay via deldir::deldir()
del_dd <- createNetwork(
    x = as.matrix(sl[, .(sdimx, sdimy)]),
    type = "delaunay",
    method = "deldir",
    include_weight = TRUE,
    weight_fun = function(d) 1 / d,
    as.igraph = FALSE,
    node_ids = sl$cell_ID
)

# kNN spatial network
kNN_spat <- createNetwork(
    x = as.matrix(sl[, .(sdimx, sdimy)]),
    type = "kNN",
    method = "dbscan",
    include_weight = TRUE,
    weight_fun = function(d) 1 / d, # not the default
    as.igraph = FALSE,
    node_ids = sl$cell_ID,
    k = 4L,
    maximum_distance = NULL,
    minimum_k = 0L
)

# kNN NN network
kNN <- createNetwork(
    pca[, seq_len(10)],
    type = "kNN",
    method = "dbscan",
    node_ids = rownames(pca),
    as.igraph = TRUE
)

# sNN NN network
sNN <- createNetwork(
    pca[, seq_len(10)],
    type = "sNN",
    method = "dbscan",
    node_ids = rownames(pca),
    as.igraph = TRUE
)

# using defaults for sNN with index IDs to create igraph
sNN_idx <- createNetwork(pca[, seq_len(10)])
```
