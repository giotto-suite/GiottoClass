# set_default_feat_type

Function to guess a default feature type. Also see
[`activeFeatType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeFeatType-generic.md)
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

set_default_feat_type(gobject = g, spat_unit = "cell", feat_type = "rna")
```
