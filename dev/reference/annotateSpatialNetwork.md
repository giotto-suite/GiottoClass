# annotateSpatialNetwork

Attach cell-level information to the ends of a spatial network's edges.

A network stores only edges: since GiottoClass 0.6.0 a
[spatialNetworkObj](https://giotto-suite.github.io/GiottoClass/dev/reference/spatialNetworkObj-class.md)
holds an igraph whose vertices carry a name and nothing else, so
anything about the cells an edge runs between has to be looked up and
attached. Both annotations this offers are the same operation on
different sources – take a cell-keyed value and write it onto each end
of the edge – and both are optional, so a caller pays only for what it
reads.

- `cluster_column` attaches a label, resolved through
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md),
  and derives the interaction columns from it (see Value).

- `coordinates` attaches the endpoint positions, read live from the
  spatial locations so that any transform already applied to them is
  carried along.

Nodes are not part of the result and cannot be: an edge table has no row
for a cell with no edges. The node set belongs to the spatial locations,
which is also where a caller drawing this should take it from.

## Usage

``` r
annotateSpatialNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spatial_network_name = "Delaunay_network",
  cluster_column = NULL,
  create_full_network = FALSE,
  coordinates = TRUE,
  spat_loc_name = NULL,
  ...
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spatial_network_name:

  name of spatial network to use

- cluster_column:

  character. Name of a cell-level value to label each edge end with.
  Resolved with
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md),
  so it may name a cell metadata column, an expression feature, an
  enrichment or any other slot that function searches. `NULL` (default)
  attaches no label and skips the lookup entirely.

- create_full_network:

  convert from reduced to full network representation

- coordinates:

  logical. Attach endpoint coordinates as `sdim[xyz]_begin` /
  `sdim[xyz]_end`. `TRUE` by default, since a network carries no
  coordinates of its own and consumers that draw edges as segments need
  them.

- spat_loc_name:

  name of the spatial locations to read endpoint coordinates from.
  `NULL` uses the default set.

- ...:

  additional arguments passed to
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  when `cluster_column` is given, e.g. `expression_values` or
  `spat_enr_name` to scope where the label is looked up.

## Value

`data.table` of edges. Always `from`, `to` and the edge attributes the
network carries. With `coordinates`, the `sdim*_begin` / `sdim*_end`
columns. With `cluster_column`, `from_cell_type` and `to_cell_type`,
plus `type_int` (`"homo"` / `"hetero"`), `from_to` (direction-specific)
and `unified_int` (direction-agnostic).

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

# labels and coordinates
annotateSpatialNetwork(g, cluster_column = "leiden_clus")

# coordinates only -- what a plotting function wants
annotateSpatialNetwork(g)

# labels only -- what a proximity analysis wants
annotateSpatialNetwork(g, cluster_column = "leiden_clus",
    coordinates = FALSE)
```
