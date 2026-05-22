# Create a spatial Delaunay network

Create a spatial Delaunay network based on cell centroid physical
distances.

## Usage

``` r
createSpatialDelaunayNetwork(
  gobject,
  name = "Delaunay_network",
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = NULL,
  method = c("deldir", "delaunayn_geometry", "RTriangle"),
  dimensions = "all",
  maximum_distance = "auto",
  minimum_k = 0,
  options = "Pp",
  Y = TRUE,
  j = TRUE,
  S = 0,
  verbose = TRUE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  ...
)
```

## Arguments

- gobject:

  giotto object

- name:

  name for spatial network (default = 'delaunay_network')

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations

- method:

  package to use to create a Delaunay network

- dimensions:

  which spatial dimensions to use. Use "sdimx" (spatial dimension x),
  "sdimy", "sdimz" respectively to refer to X (or the 1st), Y (or the
  2nd) and Z(or the 3rd) dimension, see details. (default = all)

- maximum_distance:

  distance cuttof for Delaunay neighbors to consider. If "auto", "upper
  whisker" value of the distance vector between neighbors is used; see
  the [`graphics::boxplot()`](https://rdrr.io/r/graphics/boxplot.html)
  documentation for more details.(default = "auto")

- minimum_k:

  minimum number of neighbours if maximum_distance != NULL

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

- verbose:

  be verbose

- return_gobject:

  logical. return giotto object (default = TRUE)

- output:

  character. Object type to return spatial network as when
  `return_gobject = FALSE`. (default: 'spatialNetworkObj')

- ...:

  Other additional parameters

## Value

giotto object with updated spatial network slot

## Details

Creates a spatial Delaunay network as explained in
[`delaunayn`](https://rdrr.io/pkg/geometry/man/delaunayn.html)
(default), [`deldir`](https://rdrr.io/pkg/deldir/man/deldir.html), or
[`triangulate`](https://rdrr.io/pkg/RTriangle/man/triangulate.html).

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

createSpatialDelaunayNetwork(g)
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
