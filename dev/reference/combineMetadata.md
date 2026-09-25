# combineMetadata

This function combines the cell metadata with spatial locations and
enrichment results from runSpatialEnrich.

## Usage

``` r
combineMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  spat_enr_names = NULL,
  verbose = TRUE,
  view = NULL,
  space = NULL
)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations to include

- spat_enr_names:

  names of spatial enrichment results to include

- verbose:

  verbosity

- view, space:

  `character`. Optional name of a view / space registered on `gobject`.
  The object is pre-narrowed once before any slot is read, so the
  returned table reflects the view-scoped subset in the frame `space`
  names.

## Value

Extended cell metadata in data.table format.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

combineMetadata(g)
```
