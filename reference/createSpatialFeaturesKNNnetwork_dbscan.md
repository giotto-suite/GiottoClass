# Create kNN spatial feature network using dbscan

to create a feature kNN spatial network using dbscan

## Usage

``` r
createSpatialFeaturesKNNnetwork_dbscan(
  gobject,
  feat_type = NULL,
  name = "knn_feats_network",
  k = 4,
  maximum_distance = NULL,
  minimum_k = 0,
  add_feat_ids = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- name:

  name to assign generated feature network

- k:

  number of neighbors for kNN to find

- maximum_distance:

  network maximum distance allowed

- minimum_k:

  minimum neighbors allowed

- add_feat_ids:

  boolean. whether to add feature information

- verbose:

  be verbose

- ...:

  additional parameters to pass to
  [`kNN`](https://rdrr.io/pkg/dbscan/man/kNN.html)

## Value

kNN spatial feature network
