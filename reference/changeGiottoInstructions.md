# deprecated

Function to change one or more instructions from giotto object. If more
than one item is supplied to `params` and `new_values`, use a vector of
values. Does not call `initialize` on the giotto object

## Usage

``` r
changeGiottoInstructions(
  gobject,
  params = NULL,
  new_values = NULL,
  return_gobject = TRUE,
  init_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- params:

  parameter(s) to change

- new_values:

  new value(s) for parameter(s)

- return_gobject:

  (boolean, default = TRUE) return giotto object

- init_gobject:

  (boolean, default = TRUE) initialize gobject if returning

## Value

giotto object with one or more changed instructions

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

changeGiottoInstructions(
    gobject = g, params = "save_plot",
    new_values = TRUE
)
#> Warning: `changeGiottoInstructions()` was deprecated in GiottoClass 0.3.5.
#> ℹ Please use `instructions()` instead.
#> An object of class giotto 
#> >Active spat_unit:  cell 
#> >Active feat_type:  rna 
#> dimensions    : 634, 624 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : cell 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [cell][rna] raw normalized scaled
#> spatial locations ----------------
#>   [cell] raw
#> spatial networks -----------------
#>   [cell] Delaunay_network spatial_network
#> spatial enrichments --------------
#>   [cell][rna] cluster_metagene DWLS
#> dim reduction --------------------
#>   [cell][rna] pca custom_pca umap custom_umap tsne
#> nearest neighbor networks --------
#>   [cell][rna] sNN.pca custom_NN
#> attached images ------------------
#> images      : alignment image 
#> 
#> 
#> Use objHistory() to see steps and params used
```
