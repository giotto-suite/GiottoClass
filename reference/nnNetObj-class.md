# S4 nnNetObj

Framework to store nearest neighbor network information

## Value

nnNetObj

## Slots

- `name`:

  name of nnNetObj

- `nn_type`:

  type of nearest neighbor network

- `igraph`:

  igraph object containing network information

- `feat_type`:

  feature type of data

- `spat_unit`:

  spatial unit of data

- `provenance`:

  origin of aggregated information (if applicable)

- `misc`:

  misc

## Examples

``` r
GiottoData::loadSubObjectMini("nnNetObj")
#> An object of class nnNetObj : "sNN.pca"
#> --| Contains nearest neighbor network generated with: sNN 
#> ----| for feat_type: rna 
#> ----|     spat_unit: aggregate 
#> ----|     provenance: z0 z1 
#> 
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
