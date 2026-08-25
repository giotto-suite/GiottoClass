# Add feature metadata

Adds feature metadata to the giotto object

## Usage

``` r
addFeatMetadata(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  new_metadata,
  vector_name = NULL,
  by_column = FALSE,
  column_feat_ID = NULL
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- spat_unit:

  spatial unit

- new_metadata:

  new metadata to use)

- vector_name:

  (optional) custom name if you provide a single vector

- by_column:

  merge metadata based on *feat_ID* column in
  [`fDataDT`](https://giotto-suite.github.io/GiottoClass/dev/reference/fDataDT.md)

- column_feat_ID:

  column name of new metadata to use if by_column = TRUE

## Value

giotto object

## Details

You can add additional feature metadata in several manners:

- 1\. Provide a data.table or data.frame with feature annotations in the
  same order as the *feat_ID* column in fDataDT(gobject) This is a bit
  risky and not the most recommended.

- 2\. Provide a data.table or data.frame with feature annotations and
  specify which column contains the feature IDs, these feature IDs need
  to match with the *feat_ID* column in fDataDT(gobject)

- 3\. Provide a vector or factor that is named with the feature IDs they
  correspond to. These names will be matched against the *feat_ID*
  column in fDataDT(gobject).

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

m <- fDataDT(g)
m <- m[, "feat_ID"]
m$new_feat_ID <- paste0("gene_", m$feat_ID)

g <- addFeatMetadata(
    g,
    new_metadata = m,
    by_column = TRUE,
    column_feat_ID = "feat_ID"
)

fDataDT(g)
```
