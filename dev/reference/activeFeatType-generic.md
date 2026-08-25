# Active feature type

Retrieve or set the active feature type. This value will be the default
feature type that the giotto object uses.

## Usage

``` r
# S4 method for class 'giotto'
activeFeatType(gobject)

# S4 method for class 'giotto,character'
activeFeatType(gobject) <- value
```

## Arguments

- gobject:

  giotto object

- value:

  feat_type to set as default

## Value

active feature type

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
activeFeatType(g)
```
