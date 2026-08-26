# nnDT_to_kNN

Convert a nearest network data.table to a kNN object

## Usage

``` r
nnDT_to_kNN(nnDT)
```

## Arguments

- nnDT:

  nearest neighbor network in data.table format

## Value

kNN object

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g_nn <- getNearestNetwork(g, output = "data.table", name = "custom_NN")
#> The NN network type was not specified, default to the
#>  first: "sNN"

nnDT_to_kNN(g_nn)
#> k-nearest neighbors for 624 objects (k=3).
#> Distance metric: 
#> 
#> Available fields: dist, id, k, sort
```
