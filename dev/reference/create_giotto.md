# Create a giotto object

`giotto` objects can represent and work with datasets that are:

- Already aggregated *(in the form of a pair of expression matrix and
  spatial centroids) + (optional) associated analyses*

- Subcellular *(polygon/mask annotations and point detections e.g.
  transcripts or polygon/mask annotations and raw intensity staining
  images (e.g. protein stains)). + (optional) associated aggregate data*

- Multiple sets/combinations of the previous two belonging to the same
  experiment, organized by
  [giotto_schema](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)

A `giotto` analysis object can be generated in several ways:

- In one step *(with `createGiottoObject()` and input data)*.

- A piecewise manner where you start with just an empty `giotto` object
  that you append to *(either using `createGiottoObject()` with no
  params or a call to the class constructor function
  [`giotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md) -
  see piecewise creation section and examples)*.

- Via technology specific convenience functions *(exported from
  {Giotto})*

See
<https://giotto-suite.github.io/Giotto_website/articles/object_creation.html>
for more information. The details sections here also expand on the above
options.

## Usage

``` r
createGiottoObject(
  expression,
  expression_feat = "rna",
  spatial_locs = NULL,
  spatial_info = NULL,
  calc_poly_centroids = FALSE,
  centroids_to_spatlocs = FALSE,
  feat_info = NULL,
  cell_metadata = NULL,
  feat_metadata = NULL,
  spatial_network = NULL,
  spatial_grid = NULL,
  spatial_grid_name = NULL,
  spatial_enrichment = NULL,
  dimension_reduction = NULL,
  nn_network = NULL,
  images = NULL,
  largeImages = NULL,
  offset_file = NULL,
  instructions = NULL,
  cores = determine_cores(),
  raw_exprs = NULL,
  expression_matrix_class = deprecated(),
  backend = NULL,
  h5_file = deprecated(),
  verbose = FALSE
)

createGiottoObjectSubcellular(
  gpolygons = NULL,
  polygon_mask_list_params = NULL,
  polygon_dfr_list_params = NULL,
  gpoints = NULL,
  cell_metadata = NULL,
  feat_metadata = NULL,
  spatial_network = NULL,
  spatial_network_name = NULL,
  spatial_grid = NULL,
  spatial_grid_name = NULL,
  spatial_enrichment = NULL,
  spatial_enrichment_name = NULL,
  dimension_reduction = NULL,
  nn_network = NULL,
  images = NULL,
  largeImages = NULL,
  largeImages_list_params = NULL,
  instructions = NULL,
  cores = NA,
  verbose = FALSE
)
```

## Arguments

- expression:

  expression information

- expression_feat:

  available features (e.g. rna, protein, ...)

- spatial_locs:

  data.table or data.frame with coordinates for cell centroids

- spatial_info:

  list of giotto polygon objects with spatial information, see
  [`createGiottoPolygonsFromMask`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  and
  [`createGiottoPolygonsFromDfr`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)

- calc_poly_centroids:

  if spatial_info is provided, whether to also calculate centroids

- centroids_to_spatlocs:

  if spatial_info is provided, whether to also convert centroids to
  spatial locations

- feat_info:

  list of giotto point objects with feature info, see
  [`createGiottoPoints`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPoints.md)

- cell_metadata:

  cell annotation metadata

- feat_metadata:

  feature annotation metadata for each unique feature

- spatial_network:

  list of spatial network(s)

- spatial_grid:

  list of spatial grid(s)

- spatial_grid_name:

  list of spatial grid name(s)

- spatial_enrichment:

  list of spatial enrichment score(s) for each spatial region

- dimension_reduction:

  list of dimension reduction(s)

- nn_network:

  list of nearest neighbor network(s)

- images:

  list of images

- largeImages:

  deprecated

- offset_file:

  file used to stitch fields together (optional)

- instructions:

  list of instructions or output result from
  [`createGiottoInstructions`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)

- cores:

  how many cores or threads to use to read data if paths are provided

- raw_exprs:

  deprecated, use expression

- expression_matrix_class:

  deprecated. See
  [`?createExprObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/createExprObj.md)
  for details

- backend:

  backend to use (optional). One of:

  - `NULL` - (default) in memory object

  - `filepath` - path at which to set up a project directory. Does not
    have to already exist. Sets the backend manager as `gDirSource` by
    default.

  - `gsource`-inheriting - A specific backend manager to use

  `filepath` and `gsource` both require GiottoDisk.

- h5_file:

  deprecated. Use `backend` instead

- verbose:

  be verbose when building Giotto object

- gpolygons:

  giotto polygons

- polygon_mask_list_params:

  list parameters for
  [`createGiottoPolygonsFromMask`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)

- polygon_dfr_list_params:

  list parameters for
  [`createGiottoPolygonsFromDfr`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)

- gpoints:

  giotto points

- spatial_network_name:

  list of spatial network name(s)

- spatial_enrichment_name:

  list of spatial enrichment name(s)

- largeImages_list_params:

  image params when loading largeImages as list

## Value

`giotto` object

## single step creation (conventional)

\[**Requirements**\] To use this method, you need to provide at least a
matrix with genes as row names and cells as column names. This matrix
can be provided as a base matrix, sparse Matrix, data.frame, data.table
or as a path to any of those. To include spatial information about cells
(or regions) you need to provide a matrix, data.table or data.frame (or
path to them) with coordinates for all spatial dimensions. This can be
2D (x and y) or 3D (x, y, x). The row order for the cell coordinates
should be the same as the column order for the provided expression data.

\[**Instructions**\] Additionally an instruction file, generated
manually or with
[`createGiottoInstructions`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
can be provided to instructions, if not a default instruction file will
be created for the Giotto object.

\[**Multiple fields**\] In case a dataset consists of multiple fields,
like seqFISH+ for example, an offset file can be provided to stitch the
different fields together.
[`stitchFieldCoordinates`](https://giotto-suite.github.io/GiottoClass/dev/reference/stitchFieldCoordinates.md)
can be used to generate such an offset file.

\[**Processed data**\] Processed count data, such as normalized data,
can be provided using one of the different expression slots (norm_expr,
norm_scaled_expr, custom_expr).

\[**Metadata**\] Cell and gene metadata can be provided using the cell
and gene metadata slots. This data can also be added afterwards using
the
[`addFeatMetadata`](https://giotto-suite.github.io/GiottoClass/dev/reference/addFeatMetadata.md)
or
[`addCellMetadata`](https://giotto-suite.github.io/GiottoClass/dev/reference/addCellMetadata.md)
functions.

\[**Other information**\] Additional information can be provided through
the appropriate slots:

- spatial networks

- spatial grids

- spatial enrichments

- dimensions reduction

- nearest neighbours networks

- images

`createGiottoObjectSubcellular()` is another variant of this
functionality that specializes in starting from polygon (e.g. cell
annotations) and points (e.g. transcripts) information, but it is a
legacy function. It is usually most convenient to work with piecewise
creation.

## piecewise creation (most flexible)

Giotto converts input data into compatible subobjects (`exprObj`,
`giottoPolygon`, `giottoPoints`, etc). These subobjects can be created
using the relevant `create*` functions. They can then directly be used
to assemble a `giotto` object.

1.  Create an empty `giotto` object (either use `createGiottoObject()`
    with no params or the class constructor
    [`giotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto-class.md))

2.  Either append created subobjects one by one or all at once as a
    `list` of subobjects.


    # pseudocode with giotto object and created subobjects:

    # Start with an empty giotto object
    g <- giotto()

    # Data (subobjects) can then be added one by one.
    g <- setGiotto(g, polys)
    g <- setGiotto(g, transcripts)
    g <- setGiotto(g, imgs)
    g <- setGiotto(g, expression)
    g <- setGiotto(g, cell_meta)

    # alternatively as a list
    g <- setGiotto(g, list(polys, transcripts, imgs, expression, cell_meta))

## technology specific

There are several convenience functions we provide for loading in data
from popular platforms. These functions are exported from {Giotto}
instead of this package. They take care of reading the expected output
folder structures, auto-detecting where needed data items are,
formatting items for ingestion, then object creation.


    # some examples are:
    Giotto::createGiottoVisiumObject()
    Giotto::createGiottoXeniumObject()
    Giotto::createGiottoVisiumHDObject()
    Giotto::createGiottoCosMxObject()

Also see
<https://giotto-suite.github.io/Giotto_website/articles/import_utilities.html>
for technology-specific import utilities that can be used with the
piecewise `giotto` object assembly method.

## Examples

``` r
# create an empty object
g <- createGiottoObject
# (can also use the class generator function)
g <- giotto()

# create an object containing an expression matrix
expr_matrix <- readRDS(system.file("extdata/toy_matrix.RDS",
    package = "GiottoClass"
))

createGiottoObject(expression = expr_matrix)

x_gpolygons <- GiottoData::loadSubObjectMini("giottoPolygon")
x_gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

createGiottoObjectSubcellular(
    gpolygons = x_gpolygons,
    gpoints = x_gpoints
)
```
