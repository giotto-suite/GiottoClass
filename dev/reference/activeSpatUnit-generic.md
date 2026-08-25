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
activeSpatUnit(g)
```
