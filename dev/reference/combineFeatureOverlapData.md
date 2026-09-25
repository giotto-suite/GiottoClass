# combineFeatureOverlapData

combine feature data information

## Usage

``` r
combineFeatureOverlapData(
  gobject,
  feat_type = "rna",
  sel_feats = NULL,
  poly_info = "cell",
  view = NULL,
  space = NULL
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- sel_feats:

  selected features (default: NULL or no selection)

- poly_info:

  polygon information name

- view, space:

  optional
  [giottoView](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  /
  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  or the name of one slotted on `gobject`. Threaded through the
  underlying getFeatureMetadata / getPolygonInfo / getFeatureInfo calls
  — the returned table reflects the view-scoped subset. One resolver
  pass is shared via `materialize(slots = ...)` so the predicate
  evaluates once.

## Value

data.table with combined spatial polygon information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineFeatureOverlapData(g, poly_info = "aggregate")
```
