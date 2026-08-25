# Update giotto object

Updates the giotto object for changes in structure for backwards
compatibility with earlier versions

## Usage

``` r
updateGiottoObject(gobject)
```

## Arguments

- gobject:

  giotto object to update

## Value

giotto object

## Details

Supported updates:

- 3.2.0 update adding multiomics slot

- master branch to suite - TODO

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

g <- updateGiottoObject(g)
```
