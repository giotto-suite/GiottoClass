# Aggregate Spatial Features Covered by Polygon Geometries

Aggregate features (either `feat_info` OR `image_names`) with polygons
(`spat_info`). Under the hood, this is performed in two steps:

1.  Find the overlapped features via the lower-level generic
    [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/reference/calculateOverlap.md)

2.  Summarize the overlapped features as a matrix via
    [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoClass/reference/overlapToMatrix.md)

## Usage

``` r
aggregateFeatures(
  gobject,
  spat_info = NULL,
  feat_info = NULL,
  image_names = NULL,
  new_spat_unit = NULL,
  new_feat_type = NULL,
  name = "raw",
  poly_subset_ids = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  fun = "sum",
  return_gobject = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  `giotto` object containing spatial data to aggregate.

- spat_info:

  character. Name of polygon information to use to aggregate features
  with.

- feat_info:

  character. Name of feature point detections to be aggregated.

- image_names:

  character. Name of image(s) containing intensity values to be
  aggregated.

- new_spat_unit:

  character (optional). Name of spatial unit to assign the expression
  info to. Default is the same as `spat_info`.

- new_feat_type:

  character (optional). Name of feature type to assign the expression
  info to. Default is the same as `feat_info` when used. When
  `image_names` is used instead, default is "protein".

- name:

  character. (default = "raw") Name to assign the output expresssion
  values information.

- poly_subset_ids:

  character vector. (optional) Specific poly_IDs to use

- feat_subset_column:

  character. (optional) feature info attribute to subset feature points
  on when performing overlap calculation.

- feat_subset_values:

  (optional) values matched against in `feat_subset_column` in order to
  subset feature points when performing overlap calculation.

- feat_count_column:

  character. (optional) feature info column with counts information.
  Useful in cases when more than one detection is reported per point.

- fun:

  character (default = "sum"). A function usable by
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  to aggregate image intensity values.

- return_gobject:

  logical (default = TRUE). Whether to return the `giotto` object or
  just the aggregated expression values as `exprObj` class.

- verbose:

  logical. Whether to be verbose.

- ...:

  Additional params to pass to the overlap calculation method. None
  implemented for point overlaps. For intensity overlaps, passes to
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  and additionally the function requested with the `fun` param.

## Value

`giotto` when `return_gobject=TRUE`, `exprObj` when
`return_gobject=FALSE`

## Details

`feat_subset_column`, `feat_subset_values`, and `feat_count_column` are
specific to overlaps on feature points info, and should not be provided
when overlapping image data.
