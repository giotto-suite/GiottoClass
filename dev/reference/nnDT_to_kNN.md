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
g_nn <- getNearestNetwork(g, output = "data.table", name = "custom_NN")

nnDT_to_kNN(g_nn)
```
