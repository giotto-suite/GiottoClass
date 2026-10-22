# Active spatial unit

Retrieve or set the active spatial unit. This value will be the default
spatial unit that the giotto object uses.

## Usage

``` r
# S4 method for class 'gAny'
activeSpatUnit(gobject)

# S4 method for class 'gAny,character'
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
