# combineSpatialCellMetadataInfo

Combine cell metadata with spatial cell information (e.g. polygon)

## Usage

``` r
combineSpatialCellMetadataInfo(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
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

  feature type(s)

- view, space:

  optional
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  /
  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  or the name of one slotted on `gobject`. Threaded through to the
  underlying `getPolygonInfo` and `getCellMetadata` calls so the
  returned table reflects the view-scoped subset.

## Value

list of data.table(s)

## Details

The returned data.table has the following columns:  

- sdimx: spatial feature location on the x-axis

- sdimy: spatial feature location on the y-axis

- cell_ID: unique cell ID

- feat: selected feature(s)

- other columns that are part of the cell metadata

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineSpatialCellMetadataInfo(g, spat_unit = "aggregate", feat_type = "rna")
```
