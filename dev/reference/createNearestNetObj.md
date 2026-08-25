# Create S4 nnNetObj

Create an S4 nnNetObj

## Usage

``` r
createNearestNetObj(
  name = "test",
  network,
  nn_type = NULL,
  spat_unit = "cell",
  feat_type = "rna",
  provenance = NULL,
  misc = NULL
)
```

## Arguments

- name:

  name of nnNetObj

- network:

  igraph object or data.frame containing nearest neighbor information
  (see details)

- nn_type:

  type of nearest neighbor network

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- provenance:

  origin of aggregated information (if applicable)

- misc:

  misc

## Value

nnNetObj

## Details

igraph and dataframe-like inputs must include certain information. For
igraph, it must have, at minimum vertex 'name' attributes and 'distance'
edge attribute. dataframe-like inputs must have 'from', 'to', and
'distance' columns

## Examples

``` r
x <- GiottoData::loadSubObjectMini("nnNetObj")

createNearestNetObj(
    network = slot(x, "network"), name = "sNN",
    nn_type = "sNN"
)
```
