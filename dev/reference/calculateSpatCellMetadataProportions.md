# calculateSpatCellMetadataProportions

calculates a proportion table for a cell metadata column (e.g. cluster
labels) for all the spatial neighbors of a source cell. In other words
it calculates the niche composition for a given annotation for each
cell.

## Usage

``` r
calculateSpatCellMetadataProportions(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_network = NULL,
  metadata_column = NULL,
  name = "proportion",
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_network:

  spatial network

- metadata_column:

  metadata column to use

- name:

  descriptive name for the calculated proportions

- return_gobject:

  return giotto object

## Value

giotto object (default) or enrichment object if return_gobject = FALSE

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

calculateSpatCellMetadataProportions(g,
    spat_network = "Delaunay_network", metadata_column = "leiden_clus"
)
```
