# Active spatial unit

Retrieve or set the active spatial unit. This value will be the default
spatial unit that the giotto object uses.

## Usage

``` r
# S4 method for class 'giotto'
activeSpatUnit(gobject)

# S4 method for class 'giotto,character'
activeSpatUnit(gobject) <- value
```

## Arguments

- gobject:

  giotto object

- value:

  spat_unit to set as default

## Value

active spatial unit

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
activeSpatUnit(g)
#> [1] "cell"
```
