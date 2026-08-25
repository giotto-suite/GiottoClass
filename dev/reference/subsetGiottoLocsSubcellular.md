# Subset raw subcellular information by location

Subsets Giotto object based on spatial coordinates. This subset function
is intended for poly_info and feat_types for which there are no matching
aggregated information.

## Usage

``` r
subsetGiottoLocsSubcellular(
  gobject,
  poly_info = NULL,
  feat_type = NULL,
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = NULL,
  z_max = NULL,
  z_min = NULL,
  verbose = FALSE
)
```

## Arguments

- gobject:

  giotto object

- poly_info:

  character. which polygons to spatially subset

- feat_type:

  character. which feature info to spatially subset

- x_max, x_min, y_max, y_min, z_max, z_min:

  minimum and maximum x, y, and z coordinates to subset to

- verbose:

  be verbose

## Value

giotto object

## Details

This is separate from
[subsetGiottoLocs](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocs.md)
because spat_units with attached aggregated information should be
removed using a cell_ids-based approach in order to ensure that paired
information is removed together. Polygons and points information without
associated aggregated values do not have this issue and can be directly
spatially subset.
