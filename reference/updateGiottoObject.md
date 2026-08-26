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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

g <- updateGiottoObject(g)
```
