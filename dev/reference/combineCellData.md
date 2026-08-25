# combineCellData

Produce a table of information about the cells, including the geometry
and centroids information. This function will be simplified in the
future with
[`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md).

## Usage

``` r
combineCellData(
  gobject,
  feat_type = "rna",
  include_spat_locs = TRUE,
  spat_loc_name = "raw",
  include_poly_info = TRUE,
  poly_info = "cell",
  include_spat_enr = TRUE,
  spat_enr_names = NULL,
  ext = NULL,
  xlim = NULL,
  ylim = NULL,
  remove_background_polygon = TRUE
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- include_spat_locs:

  include information about spatial locations

- spat_loc_name:

  spatial location name

- include_poly_info:

  include information about polygon

- poly_info:

  polygon information name

- include_spat_enr:

  include information about spatial enrichment

- spat_enr_names:

  names of spatial enrichment results to include

- ext:

  numeric or SpatExtent (optional). A cropping extent to apply to to the
  geometries.

- xlim, ylim:

  numeric length of 2 (optional). x or y bounds to apply.

- remove_background_polygon:

  logical (default = `TRUE`).
  [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  may sometimes produce extent-filling polygons when the original
  geometry is problematic or invalid. Set `TRUE` to remove these, based
  on whether a polygon fills up most of the x and y range.

## Value

data.table with combined spatial information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineCellData(g, poly_info = "aggregate")
```
