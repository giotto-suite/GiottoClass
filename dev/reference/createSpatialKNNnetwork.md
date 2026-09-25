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
  name = NULL,
  default_name = "knn_network",
  k = 4,
  maximum_distance = NULL,
  minimum_k = 0,
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

- default_name:

  name to fall back on when `name` is `NULL`, before the coordinate
  frame is prefixed. Exists so that one composition site can serve every
  entry point while each keeps its own spelling; callers do not normally
  set it.

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

  additional arguments to the selected method function

## Value

giotto object with updated spatial network slot

**dimensions:** default = 'all' which takes all possible dimensions.
Alternatively you can provide a character vector that specififies the
spatial dimensions to use, e.g. c("sdimx', "sdimy") or a numerical
vector, e.g. 2:3

**maximum_distance:** this is a post-filter on the k neighbours the
search already found, not a constraint on the search itself. To build a
network on distance alone, prefer
[`radiusNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/radiusNetworkParam-class.md),
which searches only within the radius; the older advice of setting `k`
very high (e.g. `k = 100`) and filtering still works, but it finds a
hundred neighbours per cell in order to discard most of them.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

createSpatialKNNnetwork(g)
```
