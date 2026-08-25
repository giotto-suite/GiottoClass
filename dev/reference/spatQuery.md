# Spatial Query

Select spatial geometries based on a list of spatial `filters`. The
final item in provided in the list is the layer of information being
queried.  
By default, results will be returned as a new polygon-based spatial unit
with selection information recorded in the associated cell metadata.  
Spatial queries may perform queries on the geometries themselves, so
[`intersect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatial_binary_ops.md)
operations are performed under the hood. For a lighter weight option
that just finds spatial relationships, see
[`relate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md)

## Usage

``` r
spatQuery(
  gobject,
  filters,
  name = "query_polys",
  clip = TRUE,
  use_centroids = NULL,
  buffer = 0,
  make_valid = FALSE,
  combine_fragments = FALSE,
  dissolve = FALSE,
  return_table = FALSE,
  return_ids = FALSE,
  return_gobject = TRUE,
  verbose = NULL
)

spatQueryGiottoPolygons(
  gobject,
  filters,
  name = "query_polys",
  clip = TRUE,
  use_centroids = NULL,
  buffer = 0,
  make_valid = FALSE,
  combine_fragments = FALSE,
  dissolve = FALSE,
  return_table = FALSE,
  return_ids = FALSE,
  return_gobject = TRUE,
  verbose = NULL
)
```

## Arguments

- gobject:

  `giotto` object

- filters:

  named list of characters and/or `giottoPolygons` to use as spatial
  filters for the final item in the list.

  - \[`character`\] list name designates the gobject spatial unit to use
    as a filter. The actual character values should either be `"all"` or
    a specific vector of cell_IDs to use.

  - \[`giottoPolygon`\] inputs are directly used as filters. List names
    are used when reporting the spatial relationships in output geometry
    objects. These can also be used as centroids and additionally can be
    buffered.

  - \[`SpatVector`\] inputs are directly used. Can also be converted to
    centroids and/or buffered.

  - \[`numeric`\] input is read as XY pairs (e.g.
    `c(x1, y1, x2, y2, ...)`), to be used as centroids. These are
    bufferable.

  - \['spatLocsObj'\] inputs are directly used as centroids. These are
    bufferable.

- name:

  (optional) character. If not `NULL`, a new spatial unit of this name
  will be generated from the results.

- clip:

  logical. Default = `TRUE`. Whether final round of querying should
  produce polygons clipped by the polygons used to select them.

- use_centroids:

  character vector. Values must correspond to names in `filters`.
  Selected `filters` will be converted to centroids. (prefers usage of
  the first set of spatlocs for that spat_unit)

- buffer:

  numeric. Or named vector of numerics. Names must correspond to those
  in `centroids`. Applies the specified buffer to the centroid to allow
  it to be used in `filter`. A `0` will skip buffering, but this is only
  permitted if is also the the last item in `filter`. Unbuffered points
  may only return results as IDs (`return_ids = TRUE`). Do note that
  buffering on a large number of elements can cause significant
  slowdowns.

- make_valid:

  logical (default = `FALSE`). Whether to make geometries valid before
  using them. Set `TRUE` if topology errors show up.

- combine_fragments:

  logical. (default = `FALSE`). Whether to combine geoms fragmented by
  the intersections as multipolygons based on the `poly_ID` col. If
  `TRUE`, the operation may introduce `NA`s in the spatial relationship
  information.

- dissolve:

  logical. (default = `FALSE`). If `combine_fragments = TRUE`, whether
  to also merge the multipolygon into a single polygon.

- return_table:

  logical. (Default = `FALSE`) Overrides `return__object`. If `TRUE`,
  return only the relationships as a `data.table`

- return_ids:

  logical. (Default = `FALSE`) Overrides `return_gobject`. If `TRUE`,
  return only the poly_IDs of the final entry in `filters`

- return_gobject:

  logical. (Default = `TRUE)`. Whether to return the new set of polygons
  attached to the giotto object.

- verbose:

  verbosity

## Value

`character` (IDs), `giottoPolygon`, or `giotto` depending on
`return_ids` and `return_gobject`.

## Functions

- `spatQueryGiottoPolygons()`: deprecated alias.

## See also

[`relate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
pz0 <- getPolygonInfo(g, "z0")
boxgrid <- tessellate(
    extent = ext(g),
    shape = "square",
    shape_size = 50,
    name = "boxgrid"
)
hexarray <- tessellate(
    extent = ext(g),
    shape = "hexagon",
    shape_size = 80,
    name = "hexarray"
)
g <- setGiotto(g, boxgrid)
g <- setGiotto(g, hexarray)

hex_ids <- sprintf("ID_%d", c(1, 3, 6, 8, 17, 19, 23))
box_ids <- sprintf("ID_%d", c(12, 14, 15, 16, 22, 41, 44, 45, 51, 52, 62))

g <- spatQuery(g,
    filters = list(
        hexarray = hex_ids,
        boxgrid = box_ids,
        z0 = "all"
    ),
    return_gobject = TRUE
)
# extract polys since we attached it to the giotto object
qp <- g[[, "query_polys"]][[1]]

qp2 <- spatQuery(g,
    filters = list(
        hexarray = hex_ids[3],
        boxgrid = box_ids,
        z0 = "all"
    ),
    buffer = c(hexarray = 150),
    return_gobject = FALSE
)

# check that extracted polys are being clipped as expected
plot(pz0)
plot(hexarray[hex_ids], border = "blue", add = TRUE)
plot(boxgrid[box_ids], add = TRUE, border = "red")
plot(qp, col = rainbow(20), add = TRUE) # selection by hex and box
plot(buffer(hexarray[hex_ids[3]], width = 150), add = TRUE) # buffered hex
plot(qp2, col = "black", add = TRUE) # selection by buffered hex and box

# query for polys that fall within 100 units of a point
res <- spatQuery(g,
    filters = list(
        pts = c(6500, -4900),
        z0 = "all"
    ),
    buffer = c(pts = 100),
    return_gobject = FALSE,
    make_valid = TRUE,
    clip = FALSE
)

pt_buffer <- buffer(
    as.points(createSpatLocsObj(c(6500, -4900))),
    100
)

plot(pz0)
plot(pt_buffer, add = TRUE, border = "dodgerblue") # the selecting shape.
# note that clip = FALSE for this selection
plot(res, col = "red", add = TRUE)

# only return the ids
ids <- spatQuery(g,
    filters = list(
        pts = c(6500, -4900),
        z0 = "all"
    ),
    buffer = c(pts = 100),
    return_ids = TRUE,
    make_valid = TRUE
)
head(ids)
length(ids)

# only return the table of relations
tab <- spatQuery(g,
    filters = list(
        hexarray = hex_ids,
        boxgrid = box_ids,
        z0 = "all"
    ),
    return_table = TRUE,
    make_valid = TRUE
)
force(tab)
```
