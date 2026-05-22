# deprecated

Function to display all instructions from giotto object

## Usage

``` r
showGiottoInstructions(gobject)
```

## Arguments

- gobject:

  giotto object

## Value

named vector with giotto instructions

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

showGiottoInstructions(g)
#> Warning: `showGiottoInstructions()` was deprecated in GiottoClass 0.3.5.
#> ℹ Please use `instructions()` instead.
#> <giottoInstructions>
#> python_path      : /usr/share/miniconda/envs/giotto_env/bin/python
#> show_plot        : TRUE
#> return_plot      : FALSE
#> save_plot        : FALSE
#> save_dir         : NA
#> plot_format      : png
#> dpi              : 300
#> units            : in
#> height           : 9
#> width            : 9
#> is_docker        : FALSE
#> active_spat_unit : cell
#> active_feat_type : rna
```
