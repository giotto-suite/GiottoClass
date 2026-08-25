# Read feature information

Function to read lists of feature information data and output a list of
generated giottoPoints objects

## Usage

``` r
readFeatData(data_list, verbose = TRUE)
```

## Arguments

- data_list:

  (nested) list of input data to read

- verbose:

  be verbose

## Value

list of giottoPoints

## Examples

``` r
x <- GiottoData::loadSubObjectMini("giottoPoints")

readFeatData(list(x))
```
