# set_default_feat_type

Function to guess a default feature type. Also see
[`activeFeatType()`](https://giotto-suite.github.io/GiottoClass/reference/activeFeatType-generic.md)
in methods-instructions.R for a way to manually assign this default

## Usage

``` r
set_default_feat_type(gobject, feat_type = NULL, spat_unit)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

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

set_default_feat_type(gobject = g, spat_unit = "cell", feat_type = "rna")
#> [1] "rna"
```
