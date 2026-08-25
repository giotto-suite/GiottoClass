# combineFeatureOverlapData

combine feature data information

## Usage

``` r
combineFeatureOverlapData(
  gobject,
  feat_type = "rna",
  sel_feats = NULL,
  poly_info = "cell"
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

## Value

data.table with combined spatial polygon information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

combineFeatureOverlapData(g, poly_info = "aggregate")
```
