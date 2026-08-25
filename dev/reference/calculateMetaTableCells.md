# calculateMetaTableCells

calculates the average metadata values for one or more (combined)
annotation columns.

## Usage

``` r
calculateMetaTableCells(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  value_cols = NULL,
  metadata_cols = NULL,
  spat_enr_names = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- value_cols:

  metadata or enrichment value columns to use

- metadata_cols:

  annotation columns found in `pDataDT(gobject)`

- spat_enr_names:

  which spatial enrichment results to include

## Value

data.table with average metadata values per (combined) annotation

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

calculateMetaTableCells(g,
    metadata_cols = "cell_ID",
    value_cols = "leiden_clus"
)
```
