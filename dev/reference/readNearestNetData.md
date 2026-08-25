# Read nearest neighbor network data

read nearest network results from list

## Usage

``` r
readNearestNetData(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of input data to read

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

nnNetObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("nnNetObj")

readNearestNetData(x)
```
