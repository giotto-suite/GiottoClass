# overlapToMatrix

create a count matrix based on overlap results from
[`calculateOverlap`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)

## Usage

``` r
# S4 method for class 'giotto'
overlapToMatrix(
  x,
  name = "raw",
  spat_info = NULL,
  feat_info = NULL,
  type = c("point", "intensity"),
  feat_count_column = NULL,
  fun = "sum",
  return_gobject = TRUE,
  verbose = TRUE,
  aggr_function = deprecated(),
  poly_info = deprecated(),
  count_info_column = deprecated(),
  ...
)

# S4 method for class 'giottoPolygon'
overlapToMatrix(
  x,
  feat_info = "rna",
  type = c("point", "intensity"),
  feat_count_column = NULL,
  count_info_column = deprecated(),
  ...
)

# S4 method for class 'SpatVector'
overlapToMatrix(
  x,
  col_names = NULL,
  row_names = NULL,
  feat_count_column = NULL,
  output = c("Matrix", "data.table"),
  verbose = TRUE,
  count_info_column = deprecated(),
  ...
)

# S4 method for class 'data.table'
overlapToMatrix(
  x,
  fun = "sum",
  output = c("Matrix", "data.table"),
  aggr_function = deprecated()
)

# S4 method for class 'overlapPointDT'
overlapToMatrix(
  x,
  name = "raw",
  sort = TRUE,
  feat_count_column = NULL,
  output = c("Matrix", "exprObj"),
  ...
)

# S4 method for class 'overlapIntensityDT'
overlapToMatrix(
  x,
  name = "raw",
  sort = TRUE,
  output = c("Matrix", "exprObj"),
  ...
)
```

## Arguments

- x:

  object containing overlaps info. Can be giotto object or SpatVector
  points or data.table of overlaps generated from `calculateOverlap`

- name:

  name for the overlap count matrix

- spat_info:

  character. Polygon information to use

- feat_info:

  character. Feature information to use

- type:

  character. Type of overlap data (either 'point' or 'intensity')

- feat_count_column:

  column with count information. If a column called "count" is present
  in the feature points data, it will be automatically selected.

- fun:

  character. Function to aggregate image information (default = "sum")

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

- aggr_function:

  deprecated. Use `fun` instead.

- poly_info:

  deprecated. Please use spat_info.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

- ...:

  additional params to pass to methods

- col_names, row_names:

  character vector. (optional) Set of row and col names that are
  expected to exist. This fixes the dimensions of the matrix since the
  overlaps information does not directly report rows and cols where no
  values were detected.

- output:

  data format/class to return the results as. Default is "Matrix"

- sort:

  logical (default = TRUE). Whether to perform a mixed sort on output
  matrix row and col names.

## Value

giotto object or count matrix

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
gpoly <- getPolygonInfo(g,
    name = "aggregate",
    return_giottoPolygon = TRUE
)
gpoints <- getFeatureInfo(g, return_giottoPoints = TRUE)

# calculate all transcripts overlapped
out_all <- calculateOverlap(gpoly, gpoints)

overlapToMatrix(out_all)
```
