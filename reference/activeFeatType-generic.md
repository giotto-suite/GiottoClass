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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> checking default envname 'giotto_env'
#> a system default python environment was found
#> Using python path:
#>  "/usr/share/miniconda/envs/giotto_env/bin/python"
activeFeatType(g)
#> [1] "rna"
```
