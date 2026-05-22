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
    network = slot(x, "igraph"), name = "sNN",
    nn_type = "sNN"
)
#> An object of class nnNetObj : "sNN"
#> --| Contains nearest neighbor network generated with: sNN 
#> ----| for feat_type: rna 
#> ----|     spat_unit: cell 
#> IGRAPH 859f76e DNW- 462 3021 -- 
#> + attr: name (v/c), weight (e/n), distance (e/n), shared (e/n), rank
#> | (e/n)
#> + edges from 859f76e (vertex names):
#> [1] 240649020551054330404932383065726870513->335295524680090725199231409883288198017
#> [2] 240649020551054330404932383065726870513->77151711298730578178892667721058396928 
#> [3] 240649020551054330404932383065726870513->322879686187630160969459793861944841206
#> [4] 240649020551054330404932383065726870513->31072896077757820678000109671990674373 
#> [5] 240649020551054330404932383065726870513->102184699197574201819246996094734116255
#> [6] 240649020551054330404932383065726870513->17685062374745280598492217386845129350 
#> + ... omitted several edges
#> 
#> 
```
