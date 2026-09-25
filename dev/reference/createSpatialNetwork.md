# Create spatial centroid connectivity network

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
  method = c("Delaunay", "kNN", "radius"),
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
  radius = NULL,
  verbose = FALSE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  space = NULL,
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

  which method to use to create a spatial network. One of `"Delaunay"`
  (default), `"kNN"`, or `"radius"`. `"radius"` connects every pair of
  cells closer together than `radius`, so unlike kNN it gives dense
  regions more neighbours than sparse ones.

- delaunay_method:

  method to use to generate Delaunay network. All three give the
  identical triangulation; `"delaunayn_geometry"` is far faster above a
  few thousand points (0.94 s vs ~363 s at 200,000) and is the only one
  that handles 3D. `"deldir"` remains the default for backward
  compatibility. See the *Choosing a Delaunay backend* section of
  [`createSpatialDelaunayNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialDelaunayNetwork.md).

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

- radius:

  (radius) distance cutoff, in the units of the spatial locations. Every
  pair of cells within this distance of each other is connected.
  Required when `method = "radius"`.

- verbose:

  be verbose

- return_gobject:

  logical. return giotto object (default = TRUE)

- output:

  character. Object type to return spatial network as when
  `return_gobject = FALSE`. (default: 'spatialNetworkObj')

- space:

  (`giottoMulti` only) `character(1)`. Name of a coordinate frame
  recorded on the object, or `NULL` (default) for each sample's own
  native frame. The frame decides the job: every sample it covers gets a
  network built in that frame's coordinates, written into its own
  `@spatial_network` slot — so this mutates the wrapped children.

  This is **not** a sample selector. An artifact generator takes none,
  because once the rows are in a slot nothing downstream can tell which
  the selector admitted (see `adr/0006`). To build over a subset of
  samples, record a space over them, or subset with `mg[...]` first.

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

createSpatialNetwork(g)
```
