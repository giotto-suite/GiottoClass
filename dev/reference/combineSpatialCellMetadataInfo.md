# combineSpatialCellMetadataInfo

Combine cell metadata with spatial cell information (e.g. polygon)

## Usage

``` r
combineSpatialCellMetadataInfo(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type(s)

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
