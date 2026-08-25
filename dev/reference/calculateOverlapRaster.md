# calculateOverlapRaster

calculate overlap between cellular structures (polygons) and features
(points).

## Usage

``` r
calculateOverlapRaster(
  gobject,
  name_overlap = NULL,
  spatial_info = NULL,
  poly_ID_names = NULL,
  feat_info = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  return_gobject = TRUE,
  verbose = TRUE,
  feat_subset_ids = deprecated(),
  count_info_column = deprecated()
)
```

## Arguments

- gobject:

  giotto object

- name_overlap:

  name for the overlap results (default to feat_info parameter)

- spatial_info:

  character. name polygon information

- poly_ID_names:

  (optional) list of poly_IDs to use

- feat_info:

  character. name of feature information

- feat_subset_column:

  feature info column to subset features with

- feat_subset_values:

  value(s) within feature info `feat_subset_column` to use for
  subsetting

- feat_count_column:

  column with count information (optional)

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

- feat_subset_ids:

  deprecated. Use `feat_subset_values` instead.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

## Value

giotto object or spatVector with overlapping information

## Details

Serial overlapping function.

## See also

[`.calculate_overlap_raster()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-calculate_overlap_raster.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

calculateOverlapRaster(g)
```
