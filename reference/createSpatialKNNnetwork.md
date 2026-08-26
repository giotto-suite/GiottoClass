# createSpatialKNNnetwork

Create a spatial knn network.

## Usage

``` r
createSpatialKNNnetwork(
  gobject,
  method = "dbscan",
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = NULL,
  dimensions = "all",
  name = "knn_network",
  k = 4,
  maximum_distance = NULL,
  minimum_k = 0,
  verbose = FALSE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  ...
)
```

## Arguments

- gobject:

  giotto object

- method:

  method to create kNN network

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations

- dimensions:

  which spatial dimensions to use (default = all)

- name:

  name for spatial network (default = 'spatial_network')

- k:

  number of nearest neighbors based on physical distance

- maximum_distance:

  distance cuttof for nearest neighbors to consider for kNN network

- minimum_k:

  minimum nearest neigbhours if maximum_distance != NULL

- verbose:

  verbose

- return_gobject:

  boolean: return giotto object (default = TRUE)

- output:

  character. Object type to return spatial network as when
  `return_gobject = FALSE`. (default: 'spatialNetworkObj')

- ...:

  additional arguments to the selected method function

## Value

giotto object with updated spatial network slot

**dimensions:** default = 'all' which takes all possible dimensions.
Alternatively you can provide a character vector that specififies the
spatial dimensions to use, e.g. c("sdimx', "sdimy") or a numerical
vector, e.g. 2:3

**maximum_distance:** to create a network based on maximum distance
only, you also need to set k to a very high value, e.g. k = 100

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

createSpatialKNNnetwork(g)
#> Setting spatial network [cell] knn_network
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
#>   [cell] Delaunay_network spatial_network knn_network
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
