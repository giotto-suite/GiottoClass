# Spatial relationships between geometries

`relate()` returns a logical matrix indicating the presence or absence
of a specific spatial relationships between the geometries in x and y.

## Usage

``` r
# S4 method for class 'giottoSpatial,giottoSpatial'
relate(
  x,
  y,
  relation,
  pairs = TRUE,
  na.rm = TRUE,
  output = c("data.table", "matrix"),
  use_names = TRUE,
  ...
)

# S4 method for class 'giotto,giottoSpatial'
relate(
  x,
  y,
  ...,
  what = c("polygon", "spatlocs", "points"),
  spat_unit = NULL,
  feat_type = NULL,
  spat_locs_name = NULL
)
```

## Arguments

- x:

  spatial object with records to test

- y:

  spatial object records to test relations against

- relation:

  character. One of "intersects", "touches", "crosses", "overlaps",
  "within", "contains", "covers", "coveredby", "disjoint", or "equals".
  It can also be a "DE-9IM" string such as "FF\*FF\*\*\*\*". See
  [Wikipedia](https://en.wikipedia.org/wiki/DE-9IM) or [GeoTools
  doc](https://docs.geotools.org/stable/userguide/library/jts/dim9.html)

- pairs:

  logical. If `TRUE` a two-column matrix is returned with the indices of
  the cases where the requested relation is `TRUE`. This is especially
  helpful when dealing with many geometries as the returned value is
  generally much smaller

- na.rm:

  logical. If `TRUE` and `pairs=TRUE`, geometries in `x` for which there
  is no related geometry in `y` are omitted

- output:

  character. `"data.table"` or `"matrix"`. `"data.table"` is only
  possible when `pairs=TRUE`

- use_names:

  logical. If `TRUE`, `pairs=TRUE`, and `output="data.table"` the IDs of
  the geometries will be used.

- ...:

  additional args to pass

- what:

  character. Which type of spatial data in the `giotto` object to
  relate. One of "polygon", "spatlocs", "points"

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_locs_name:

  name of spatlocs to use if what = "spatlocs"

## Value

`data.table` if `output="data.table"`. `matrix` if `output="matrix"`

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
sl <- g[["spatial_locs"]][[1]]
gpoints <- g[["feat_info"]][[1]]
gpoly <- g[["spatial_info"]][[1]]

res1 <- relate(gpoints, gpoly, relation = "intersects")
res2 <- relate(gpoints, gpoly, relation = "intersects", use_names = FALSE)

selection <- system.file("extdata/viz_interactive_select.csv",
    package = "GiottoClass"
)
select_polys <- createGiottoPolygon(
    # we don't want the rownumber column.
    data.table::fread(selection)[, c("x", "y", "name")]
)
#>   Selecting col "name" as poly_ID column
#>   Selecting cols "x" and "y" as x and y respectively
res <- relate(g, select_polys, relation = "intersects")
g[, res[y == "polygon1", x]]
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 25 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
g[, res[y == "polygon2", x]]
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 90 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
g[, res[y == "polygon3", x]]
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 245 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
```
