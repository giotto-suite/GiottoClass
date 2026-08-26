# Create spatial network

Create a spatial network based on cell centroids. These networks are
often used when determining cell-cell connectivities and spatial
relationships. There are several types of spatial networks and multiple
methods to generate them. Method-specific params are labeled with the
name of the method within parentheses in their descriptions.

## Usage

``` r
createSpatialNetwork(
  gobject,
  name = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = NULL,
  dimensions = "all",
  method = c("Delaunay", "kNN"),
  delaunay_method = c("deldir", "delaunayn_geometry", "RTriangle"),
  maximum_distance_delaunay = "auto",
  options = "Pp",
  Y = TRUE,
  j = TRUE,
  S = 0,
  minimum_k = 0,
  knn_method = "dbscan",
  k = 4,
  maximum_distance_knn = NULL,
  verbose = FALSE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  ...
)
```

## Arguments

- gobject:

  giotto object

- name:

  name for spatial network (default = 'spatial_network')

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations to use

- dimensions:

  which spatial dimensions to use (default = all)

- method:

  which method to use to create a spatial network. (default = Delaunay)

- delaunay_method:

  method to use to generate Delaunay network

- maximum_distance_delaunay:

  distance cutoff for nearest neighbors to consider for Delaunay
  network. If "auto", "upper whisker" value of the distance vector
  between neighbors is used; see the
  [grDevices::boxplot.stats](https://rdrr.io/r/grDevices/boxplot.stats.html)
  documentation for more details.(default = "auto")

- options:

  (geometry) String containing extra control options for the underlying
  Qhull command; see the [Qhull
  documentation](http://www.qhull.org/html/qdelaun.htm) for the
  available options. (default = 'Pp', do not report precision problems)

- Y:

  (RTriangle) If TRUE prohibits the insertion of Steiner points on the
  mesh boundary.

- j:

  (RTriangle) If TRUE jettisons vertices that are not part of the final
  triangulation from the output.

- S:

  (RTriangle) Specifies the maximum number of added Steiner points.

- minimum_k:

  minimum nearest neighbours if maximum_distance != NULL

- knn_method:

  method to create kNN network

- k:

  number of nearest neighbors based on physical distance

- maximum_distance_knn:

  distance cutoff for nearest neighbors to consider for kNN network

- verbose:

  be verbose

- return_gobject:

  logical. return giotto object (default = TRUE)

- output:

  character. Object type to return spatial network as when
  `return_gobject = FALSE`. (default: 'spatialNetworkObj')

- ...:

  Additional parameters for the selected function

## Value

giotto object with updated spatial network slot

## Details

Creates a spatial network connecting single-cells based on their
physical distance to each other. For Delaunay method, neighbors will be
decided by Delaunay triangulation and a maximum distance criteria. For
kNN method, number of neighbors can be determined by k, or maximum
distance from each cell with or without setting a minimum k for each
cell.

\*\*dimensions: \*\* default = 'all' which takes all possible
dimensions. Alternatively you can provide a character vector that
specifies the spatial dimensions to use, e.g. c("sdimx', "sdimy") or a
numerical vector, e.g. 2:3

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

createSpatialNetwork(g)
#> Delaunay_network has already been used, will be overwritten
#> > " Delaunay_network " already exists and will be replaced with new spatial
#>  network
#> Setting spatial network [cell] Delaunay_network
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
