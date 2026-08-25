# Read spatial enrichment

read spatial enrichment results from list

## Usage

``` r
readSpatEnrichData(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of spatial enrichment input data

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

spatEnrObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatEnrObj")

readSpatEnrichData(x)
```
