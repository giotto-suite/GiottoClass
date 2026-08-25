# Read dimensional reduction data

read dimension reduction results from list

## Usage

``` r
readDimReducData(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  reduction = c("cells", "feats"),
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of dimension reduction input data

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- reduction:

  whether dim reduction was performed on 'cels' or 'feats'

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

dimObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("dimObj")

readDimReducData(x)
```
