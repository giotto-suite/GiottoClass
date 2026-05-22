# Giotto instructions

Giotto instructions are default settings that are applied at the
`giotto` object level. Once added to an object, they affect the way that
the object behaves. You can create a `giottoInstructions` object using
`createGiottoInstructions()` and add them to the `giotto` object during
creation or using the `instructions()` generic. Specific settings can be
replaced or retrieved using the `param` argument. Additionally, when
using `instructions<-()` as a replacement function, `initialize()` will
be called on the `giotto` object if `initialize = TRUE`.

If no `giottoInstructions` object is provided during `giotto` object
creation, then a default one will be created during `giotto` object
initialization.

## Usage

``` r
createGiottoInstructions(
  python_path = getOption("giotto.py_path"),
  show_plot = NULL,
  return_plot = NULL,
  save_plot = NULL,
  save_dir = NULL,
  plot_format = NULL,
  dpi = NULL,
  units = NULL,
  height = NULL,
  width = NULL,
  is_docker = FALSE,
  plot_count = 0,
  fiji_path = NULL,
  no_python_warn = FALSE
)

# S4 method for class 'missing,missing'
instructions(gobject, param, ...)

# S4 method for class 'giotto,missing'
instructions(gobject)

# S4 method for class 'giotto,character'
instructions(gobject, param)

# S4 method for class 'giottoInstructions,character'
instructions(gobject, param)

# S4 method for class 'giotto,missing,missing'
instructions(gobject, initialize) <- value

# S4 method for class 'giotto,missing,logical'
instructions(gobject, initialize) <- value

# S4 method for class 'giotto,character,missing'
instructions(gobject, param, initialize) <- value

# S4 method for class 'giotto,character,logical'
instructions(gobject, param, initialize) <- value

# S4 method for class 'giottoInstructions,character,ANY'
instructions(gobject, param) <- value
```

## Arguments

- python_path:

  path to python binary to use or directory one level up from the `env`
  directory (similar to output of
  [`reticulate::miniconda_path()`](https://rstudio.github.io/reticulate/reference/miniconda_path.html))

- show_plot:

  print plot to console, default = TRUE

- return_plot:

  return plot as object, default = TRUE

- save_plot:

  automatically save plot, dafault = FALSE

- save_dir:

  path to directory where to save plots

- plot_format:

  format of plots (defaults to png)

- dpi:

  resolution for raster images

- units:

  units of format (defaults to in)

- height:

  height of plots

- width:

  width of plots

- is_docker:

  using docker implementation of Giotto (defaults to FALSE)

- plot_count:

  (global option) start count for creating automatic unique plots

- fiji_path:

  path to fiji executable

- no_python_warn:

  turn off warning that no compatible python env has been detected

- gobject:

  giotto object

- param:

  Specific param in instructions to access or modify

- ...:

  params to pass to `createGiottoInstructions()`

- initialize:

  (boolean, default = TRUE) whether to initialize the giotto object

- value:

  value to set

## Value

`giottoInstructions`, instructions settings, or `giotto` objects with
modified instructions

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

# create instructions
ins <- instructions()
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

# get instructions
instrs <- instructions(g)
force(instrs)
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

# get single instructions param
instructions(g, "show_plot")
#> [1] TRUE

# replace an instruction param
instructions(g, "show_plot") <- FALSE
instructions(g, "show_plot")
#> [1] FALSE

# replace multiple instruction params
instructions(g)
#> <giottoInstructions>
#> python_path      : /usr/share/miniconda/envs/giotto_env/bin/python
#> show_plot        : FALSE
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
instructions(g, c("show_plot", "dpi")) <- list(TRUE, 600)
instructions(g)
#> <giottoInstructions>
#> python_path      : /usr/share/miniconda/envs/giotto_env/bin/python
#> show_plot        : TRUE
#> return_plot      : FALSE
#> save_plot        : FALSE
#> save_dir         : NA
#> plot_format      : png
#> dpi              : 600
#> units            : in
#> height           : 9
#> width            : 9
#> is_docker        : FALSE
#> active_spat_unit : cell
#> active_feat_type : rna

# replace instructions
i <- createGiottoInstructions()
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
instructions(g) <- i
instructions(g)
#> <giottoInstructions>
#> python_path      : /usr/share/miniconda/envs/giotto_env/bin/python
#> show_plot        : TRUE
#> return_plot      : TRUE
#> save_plot        : FALSE
#> save_dir         : /home/runner/work/GiottoClass/GiottoClass/docs/reference
#> plot_format      : png
#> dpi              : 300
#> units            : in
#> height           : 9
#> width            : 9
#> is_docker        : FALSE
#> active_spat_unit : cell
#> active_feat_type : rna
```
