# deprecated

Function to replace all instructions from giotto object. Does not call
`initialize` on the giotto object

## Usage

``` r
replaceGiottoInstructions(gobject, instructions = NULL, init_gobject = TRUE)
```

## Arguments

- gobject:

  giotto object

- instructions:

  new instructions (e.g. result from createGiottoInstructions)

- init_gobject:

  (boolean, default = TRUE) initialize gobject when returning

## Value

giotto object with replaces instructions

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

replaceGiottoInstructions(
    gobject = g,
    instructions = createGiottoInstructions()
)
#> Warning: `replaceGiottoInstructions()` was deprecated in GiottoClass 0.3.5.
#> ℹ Please use `instructions()` instead.
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
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
