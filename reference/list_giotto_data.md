# list_giotto_data

list the available data within specified giotto object slot

## Usage

``` r
list_giotto_data(gobject = NULL, slot = NULL, ...)
```

## Arguments

- gobject:

  giotto object

- slot:

  giotto object slot of interest (e.g. "expression", "spatial_locs",
  etc.)

- ...:

  additional params to pass

## Value

names and locations of data within giotto object slot

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

list_giotto_data(gobject = g, slot = "expression")
#>    spat_unit feat_type       name
#>       <char>    <char>     <char>
#> 1:      cell       rna        raw
#> 2:      cell       rna normalized
#> 3:      cell       rna     scaled
```
