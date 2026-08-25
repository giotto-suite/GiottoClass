# Create S4 spatEnrObj

Create S4 spatEnrObj

## Usage

``` r
createSpatEnrObj(
  enrichment_data,
  name = "test",
  spat_unit = "cell",
  feat_type = "rna",
  method = NULL,
  provenance = NULL,
  misc = NULL,
  verbose = TRUE
)
```

## Arguments

- enrichment_data:

  spatial enrichment results, provided a dataframe-like object

- name:

  name of S4 spatEnrObj

- spat_unit:

  spatial unit of aggregated expression (e.g. 'cell')

- feat_type:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- method:

  method used to generate spatial enrichment information

- provenance:

  origin data of aggregated expression information (if applicable)

- misc:

  misc additional information about the spatial enrichment or how it was
  generated

- verbose:

  be verbose

## Value

spatEnrObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatEnrObj")

createSpatEnrObj(
    enrichment_data = slot(x, "enrichDT"),
    name = "cluster_metagene"
)
```
