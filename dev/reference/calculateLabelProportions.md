# Calculate Proportions of Labels Per Observation Group

Calculate a proportion table for a cell metadata column (e.g. celltype
labels) based on defined groupings of cells. These groups can be defined
in one of 3 ways:

- `"table"` - explicitly provide a `data.frame` of relationships between
  a grouping column and cell IDs or provide a colname in cell metadata
  with grouping information. Method-specific params are:

  - `groups` - the `data.frame` or `character` input

  - `column_group_id` - column in `groups` defining the groups

  - `column_cell_id` - column in `groups` defining the grouped cell_IDs

- `"spatialnetwork"` - Use a spatial network to find groups of cells,
  where the groups are the cells and their network neighbors.
  Method-specific params are:

  - `spatial_network_name` - name of spatial network to use

  - `weights` - whether to consider proportion contribution of neighbors
    based on network weights (`TRUE`) or adjacency (`FALSE`)

  - `alpha` - weighting between 0 and 1 to use for the cell itself. This
    is independent from the `weights` param.

- `"polygon"` - Use a set of polygons indicated using `spat_info` to
  select underlying cells of the `spat_unit`. This is determined with
  `relate(relation = "intersects")`, where the underlying cells may be
  represented by either the polygons, their centroids, if
  `centroids = TRUE`, or the spatial locations if a specific
  `spat_loc_name` is provided.

  - `spat_info` - name of polygons to select with.

  - `select_on` - one of `"spatial_locs"` or `"polygons"`, determining
    whether the cells to be selected will be represented by their
    spatial locations (further specified via `spat_loc_name`) or their
    polygons.

  - `centroids` - if `select_on = "polygons"`, further specify whether
    to perform selection on polygon centroids.

  - `spat_loc_name` - if `select_on = "spatial_locs"`, further specify
    the set of spatial locations to use.

## Usage

``` r
calculateLabelProportions(
  gobject,
  labels,
  group_method = c("table", "spatialnetwork", "polygon"),
  groups = NULL,
  column_cell_id = "cell_ID",
  column_group_id = NULL,
  spatial_network_name = NULL,
  alpha = 1,
  weights = FALSE,
  spat_info,
  select_on = c("spatial_locs", "polygons"),
  centroids = TRUE,
  spat_loc_name = NULL,
  name = "proportions",
  spat_unit = NULL,
  feat_type = NULL,
  output = c("data.table", "matrix", "spatEnrObj", "gobject"),
  verbose = NULL
)
```

## Arguments

- gobject:

  giotto object

- labels:

  character. Metadata column gobject with labels to use

- group_method:

  character, one of `"table"`, `"spatialnetwork"`, `"polygon"`. Method
  used to find groups of cell_IDs to perform proportion calculation on.
  See description.

- groups:

  character or data.frame. If character, groups are assumed to be a
  metadata column to use. If data.frame, a 2 column table of relations
  between groups and cell_IDs in those groups. The values of group
  column will be used as the group names.

- column_cell_id:

  character. Name of column in `groups` that contains cell_ID values to
  use.

- column_group_id:

  character. Name of column in `groups` that contains the group ids. If
  not provided, the first character col in `groups` that is not
  `column_cell_id` will be used.

- spatial_network_name:

  character. Name of spatial network to use to group cell_ID values to
  use.

- alpha:

  numeric. Value between 0 and 1 inclusive that defines weighting for
  self-self network connections.

- weights:

  logical. Whether to use the `"weight"` information included with
  spatial networks as part of the proportions calculation.

- spat_info:

  character. Name of polygon information to use to group

- select_on:

  character. One of `"spatial_locs"` or `"polygons"`. Whether to perform
  the polygon grouping on the spatial locations information or the
  polygons.

- centroids:

  logical. When `select_on = "polygons"`, whether to use the polygon
  centroids instead of the polygon for the spatial intersects operation.

- spat_loc_name:

  character. Name of spatial locations to use in spatial intersects
  operation.

- name:

  character. Name to assign to the `spatEnrObj` results if `output` is
  either `"spatEnrObj"` or `"gobject"`.

- spat_unit:

  spatial unit to perform grouping selection and calculation on.

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- output:

  character. Type of data to return. One of `"data.table"`, `"matrix"`,
  `"gobject"`, or `"spatEnrObj"`

- verbose:

  verbosity.

## Value

`gobject` with `spatEnrObj` of results attached, `data.table`, `matrix`,
or `spatEnrObj` depending on `output` param.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
activeSpatUnit(g) <- "aggregate"
rels <- data.frame(
    grp = rep(LETTERS[1:10], length.out = ncol(g)),
    cid = colnames(g)
)
# calculate and return as data.table
calculateLabelProportions(g,
    labels = "leiden_clus", groups = rels, column_cell_id = "cid",
    spat_unit = "aggregate", output = "data.table"
)
# return as matrix
calculateLabelProportions(g,
    labels = "leiden_clus", groups = rels, column_cell_id = "cid",
    spat_unit = "aggregate", output = "matrix"
)
# calculate with groups from another column in metadata
calculateLabelProportions(g,
    labels = "louvain_clus", groups = "leiden_clus",
    spat_unit = "aggregate", output = "matrix"
)
# calculate proportions across all cells
calculateLabelProportions(g,
    labels = "leiden_clus",
    groups = data.frame(
        id = "all", # this is an arbitrary name
        cell_ID = colnames(g)
    ),
    spat_unit = "aggregate",
    output = "matrix"
)

# network
g <- createSpatialNetwork(g,
    name = "knn_k8_r30",
    maximum_distance_knn = 30,
    k = 8
 )
calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
    spat_unit = "aggregate", output = "spatEnrObj"
)
# add to gobject
g <- calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
    spat_unit = "aggregate", output = "gobject"
)
# with weighted contributions and alpha = 0 (no self-self contribution)
g <- calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
    spat_unit = "aggregate", output = "gobject", alpha = 0, weights = TRUE
)

# polygon
hex <- tessellate(
    extent = ext(g), shape = "hexagon", shape_size = 20, gap = -5,
    name = "hex"
)
g <- setGiotto(g, hex)
g <- calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "polygon", spat_info = "hex",
    spat_unit = "aggregate", output = "gobject"
)
```
