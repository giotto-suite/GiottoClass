# Construct a [labelProportionsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam-class.md)

Factory for the label-proportions analysis parameter object. See
[`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md)
for full descriptions of the grouping methods and their parameters.

## Usage

``` r
labelProportionsParam(
  labels,
  group_method = c("table", "spatialnetwork", "polygon"),
  groups = NULL,
  column_cell_id = "cell_ID",
  column_group_id = NULL,
  spatial_network_name = NULL,
  alpha = 1,
  weights = FALSE,
  spat_info = NULL,
  select_on = c("spatial_locs", "polygons"),
  centroids = TRUE,
  spat_loc_name = NULL,
  name = "proportions",
  ...
)
```

## Arguments

- labels:

  character. Cell metadata column with labels to compose.

- group_method:

  character. One of `"table"`, `"spatialnetwork"`, `"polygon"`. Used by
  the
  [`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
  giotto-class router to pick the primary data class to dispatch on.

- groups, column_cell_id, column_group_id:

  table-method params.

- spatial_network_name, alpha, weights:

  spatialnetwork-method params.

- spat_info, select_on, centroids, spat_loc_name:

  polygon-method params.

- name:

  character. Name to assign to result if returned as `spatEnrObj` /
  `gobject`.

- ...:

  additional named entries to attach to `@param`.

## Value

A
[labelProportionsParam](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam-class.md)
object.

## Examples

``` r
p <- labelProportionsParam(labels = "leiden_clus",
    group_method = "spatialnetwork", spatial_network_name = "knn8")
```
