# overlapImagesToMatrix

create a count matrix based on overlap results from
[`calculateOverlapPolygonImages`](https://giotto-suite.github.io/GiottoClass/reference/calculateOverlapPolygonImages.md)

## Usage

``` r
overlapImagesToMatrix(
  gobject,
  name = "raw",
  poly_info = "cell",
  feat_info = "protein",
  name_overlap = "images",
  aggr_function = "sum",
  image_names = NULL,
  spat_locs_name = "raw",
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- name:

  name for the overlap count matrix

- poly_info:

  polygon information

- feat_info:

  feature information

- name_overlap:

  name of the overlap

- aggr_function:

  function to aggregate image information (default = sum)

- image_names:

  names of images you used

- spat_locs_name:

  name for spatial centroids / locations associated with matrix

- return_gobject:

  return giotto object (default: TRUE)

## Value

giotto object or data.table with aggregated information
