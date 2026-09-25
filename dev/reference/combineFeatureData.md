# combineFeatureData

combine feature data information

## Usage

``` r
combineFeatureData(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  sel_feats = NULL,
  view = NULL,
  space = NULL
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

- view, space:

  optional
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  /
  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  or the name of one slotted on `gobject`. Threaded through to
  `getFeatureInfo` and `getFeatureMetadata` so feature-level view
  projections are applied before assembly.

## Value

data.table with combined spatial feature information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineFeatureData(g, spat_unit = "aggregate", feat_type = "rna")
```
