# combineFeatureData

combine feature data information

## Usage

``` r
combineFeatureData(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  sel_feats = NULL
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- spat_unit:

  spatial unit

- sel_feats:

  selected features (default: NULL or no selection)

## Value

data.table with combined spatial feature information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineFeatureData(g, spat_unit = "aggregate", feat_type = "rna")
```
