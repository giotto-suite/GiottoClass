# Add cell metadata

Adds cell metadata to the giotto object

## Usage

``` r
addCellMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  new_metadata,
  vector_name = NULL,
  by_column = FALSE,
  column_cell_ID = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- new_metadata:

  new cell metadata to use (data.table, data.frame, vector, factor, ...)

- vector_name:

  (optional) custom name for new metadata column if single vector or
  factor is provided

- by_column:

  merge metadata based on *cell_ID* column in
  [`pDataDT`](https://giotto-suite.github.io/GiottoClass/dev/reference/pDataDT.md)
  (default = FALSE)

- column_cell_ID:

  column name of new metadata to use if `by_column = TRUE`

## Value

giotto object

## Details

You can add additional cell metadata in several manners:

- 1\. Provide a data.frame-like object, vector, or factor with cell
  annotations in the same order as the *cell_ID* column in
  pDataDT(gobject). This is a bit risky and not the most recommended.

- 2\. Provide a data.frame-like object with cell annotations and specify
  which column contains the cell IDs, these cell IDs need to match with
  the *cell_ID* column in pDataDT(gobject)

- 3\. Provide a vector or factor that is named with the cell IDs they
  correspond to. These names will be matched against the *cell_ID*
  column in pDataDT(gobject).

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

m <- pDataDT(g)
m <- m[, c("cell_ID", "leiden_clus")]
m$cell_type <- paste0("cell_type_", m$leiden_clus)
m <- m[, c("cell_ID", "cell_type")]

g <- addCellMetadata(
    g,
    new_metadata = m,
    by_column = TRUE,
    column_cell_ID = "cell_ID"
)

pDataDT(g)
```
