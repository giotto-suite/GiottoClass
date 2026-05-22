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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
activeSpatUnit(g) <- "aggregate"
rels <- data.frame(
    grp = rep(LETTERS[1:10], length.out = ncol(g)),
    cid = colnames(g)
)
#> Getting values from [aggregate][rna] cell metadata
# calculate and return as data.table
calculateLabelProportions(g,
    labels = "leiden_clus", groups = rels, column_cell_id = "cid",
    spat_unit = "aggregate", output = "data.table"
)
#> [calculateLabelProportions] no 'column_group_id' provided.
#>  Autodetecting...'
#> [calculateLabelProportions] selecting "grp" as 'column_group_id'
#> Key: <grp>
#>        grp         1         2         3         4          5
#>     <char>     <num>     <num>     <num>     <num>      <num>
#>  1:      A 0.2978723 0.2127660 0.2553191 0.2340426 0.00000000
#>  2:      B 0.2553191 0.2765957 0.1489362 0.2127660 0.10638298
#>  3:      C 0.3043478 0.1739130 0.2826087 0.2391304 0.00000000
#>  4:      D 0.2608696 0.3478261 0.1739130 0.1739130 0.04347826
#>  5:      E 0.4130435 0.2173913 0.1304348 0.1521739 0.08695652
#>  6:      F 0.3043478 0.3043478 0.1521739 0.1521739 0.08695652
#>  7:      G 0.1956522 0.3043478 0.3260870 0.1739130 0.00000000
#>  8:      H 0.2826087 0.2608696 0.2391304 0.1739130 0.04347826
#>  9:      I 0.4130435 0.2608696 0.1521739 0.1739130 0.00000000
#> 10:      J 0.2826087 0.2826087 0.2608696 0.1521739 0.02173913
# return as matrix
calculateLabelProportions(g,
    labels = "leiden_clus", groups = rels, column_cell_id = "cid",
    spat_unit = "aggregate", output = "matrix"
)
#> [calculateLabelProportions] no 'column_group_id' provided.
#>  Autodetecting...'
#> [calculateLabelProportions] selecting "grp" as 'column_group_id'
#> 10 x 5 Matrix of class "dgeMatrix"
#>           1         2         3         4          5
#> A 0.2978723 0.2127660 0.2553191 0.2340426 0.00000000
#> B 0.2553191 0.2765957 0.1489362 0.2127660 0.10638298
#> C 0.3043478 0.1739130 0.2826087 0.2391304 0.00000000
#> D 0.2608696 0.3478261 0.1739130 0.1739130 0.04347826
#> E 0.4130435 0.2173913 0.1304348 0.1521739 0.08695652
#> F 0.3043478 0.3043478 0.1521739 0.1521739 0.08695652
#> G 0.1956522 0.3043478 0.3260870 0.1739130 0.00000000
#> H 0.2826087 0.2608696 0.2391304 0.1739130 0.04347826
#> I 0.4130435 0.2608696 0.1521739 0.1739130 0.00000000
#> J 0.2826087 0.2826087 0.2608696 0.1521739 0.02173913
# calculate with groups from another column in metadata
calculateLabelProportions(g,
    labels = "louvain_clus", groups = "leiden_clus",
    spat_unit = "aggregate", output = "matrix"
)
#> Getting values from [aggregate][rna] cell metadata
#> [calculateLabelProportions] no 'column_group_id' provided.
#>  Autodetecting...'
#> [calculateLabelProportions] selecting "leiden_clus" as 'column_group_id'
#> 5 x 30 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 30 column names ‘0’, ‘1’, ‘2’ ... ]]
#>                                                                            
#> 1 .         .          0.1510791 .         0.007194245 .        . .        
#> 2 0.1721311 0.04098361 .         0.1967213 .           0.147541 . .        
#> 3 .         .          .         .         0.102040816 .        . .        
#> 4 .         .          .         .         .           .        . 0.2235294
#> 5 .         .          .         .         .           .        1 .        
#>                                                                        
#> 1 .         .         .         .          .         .         .       
#> 2 .         .         0.1229508 0.08196721 .         .         0.147541
#> 3 .         .         .         .          0.1428571 0.1530612 .       
#> 4 0.2117647 0.1764706 .         .          .         .         .       
#> 5 .         .         .         .          .         .         .       
#>                                                                            
#> 1 .         .         .         .          0.09352518 .          0.20863309
#> 2 .         .         .         0.09016393 .          .          .         
#> 3 .         0.2142857 .         .          .          0.08163265 0.01020408
#> 4 0.2117647 .         0.1764706 .          .          .          .         
#> 5 .         .         .         .          .          .          .         
#>                                                                          
#> 1 0.0647482 0.02158273 0.2014388 0.02877698 0.1007194 0.07913669 .       
#> 2 .         .          .         .          .         .          .       
#> 3 .         0.05102041 .         .          .         .          0.244898
#> 4 .         .          .         .          .         .          .       
#> 5 .         .          .         .          .         .          .       
#>             
#> 1 0.04316547
#> 2 .         
#> 3 .         
#> 4 .         
#> 5 .         
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
#> Getting values from [aggregate][rna] cell metadata
#> [calculateLabelProportions] no 'column_group_id' provided.
#>  Autodetecting...'
#> [calculateLabelProportions] selecting "id" as 'column_group_id'
#> 1 x 5 Matrix of class "dgeMatrix"
#>             1         2         3         4          5
#> all 0.3008658 0.2640693 0.2121212 0.1839827 0.03896104

# network
g <- createSpatialNetwork(g,
    name = "knn_k8_r30",
    maximum_distance_knn = 30,
    k = 8
 )
#> Setting spatial network [aggregate] knn_k8_r30
calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "spatialnetwork", spatial_network_name = "knn_k8_r30",
    spat_unit = "aggregate", output = "spatEnrObj"
)
#> An object of class spatEnrObj : "proportions"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#>    ------------------------
#> 
#> preview:
#>            1         2         3         4     5
#>        <num>     <num>     <num>     <num> <num>
#> 1: 0.0000000 0.5000000 0.0000000 0.5000000     0
#> 2: 0.0000000 0.7142857 0.0000000 0.2857143     0
#> 3: 0.4285714 0.0000000 0.5714286 0.0000000     0
#>                                    cell_ID
#>                                     <char>
#> 1: 100210519278873141813371229408401071444
#> 2: 101161259912191124732236989250178928032
#> 3: 101488859781016188084173008420811094152
#> 
#> ...first 20 remaining colnames:
#> 
#>   
#> 
#> 
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
#> 650 polygons generated
g <- setGiotto(g, hex)
#> Setting polygon info [hex]
g <- calculateLabelProportions(g, labels = "leiden_clus",
    group_method = "polygon", spat_info = "hex",
    spat_unit = "aggregate", output = "gobject"
)
#> using hex polygons to select
#> Warning: [gobject init-check][spatial
#>  enrichments][hex][rna][proportions]
#>  Spatial enrichment IDs are not all found in gobject
#>  IDs.
```
