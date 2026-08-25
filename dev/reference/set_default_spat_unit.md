# set_default_spat_unit

Function to guess a default spatial unit. Also see
[`activeSpatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeSpatUnit-generic.md)
in methods-instructions.R for a way to manually assign this default

## Usage

``` r
set_default_spat_unit(gobject, spat_unit = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

## Value

character

## Examples

``` r
g <- createGiottoObject()

set_default_spat_unit(gobject = g, spat_unit = "cell")
```
