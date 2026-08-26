# set_default_spat_unit

Function to guess a default spatial unit. Also see
[`activeSpatUnit()`](https://giotto-suite.github.io/GiottoClass/reference/activeSpatUnit-generic.md)
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
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

set_default_spat_unit(gobject = g, spat_unit = "cell")
#> [1] "cell"
```
