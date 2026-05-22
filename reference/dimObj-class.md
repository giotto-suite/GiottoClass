# S4 dimObj Class

Framework to store dimension reduction information

## Value

dimObj

## Slots

- `name`:

  name of dimObject

- `feat_type`:

  feature type of data

- `spat_unit`:

  spatial unit of data

- `provenance`:

  origin of aggregated information (if applicable)

- `reduction`:

  whether reduction was performed on 'feats' or 'cells'

- `reduction_method`:

  method used to generate dimension reduction

- `coordinates`:

  embedding coordinates

- `misc`:

  method-specific additional outputs

## Examples

``` r
GiottoData::loadSubObjectMini("dimObj")
#> An object of class dimObj : "pca"
#> --| Contains dimension reduction generated with: pca 
#> ----| for feat_type: rna 
#> ----|     spat_unit: aggregate 
#> 
#>    100 dimensions for 462 data points
#> 
#> Additional included info:
#> [1] "eigenvalues" "loadings"   
#> 
```
