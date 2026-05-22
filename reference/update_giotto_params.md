# Update giotto parameters

For developer use. Adds an entry to the `giotto` object object history.
Care currently needs to be taken when a function that contains a call to
this function is called from within yet another function. In such cases,
a `toplevel < 0` or setting a temporary `"giotto.update_param" = FALSE`
with `GiottoUtils::gwith_option()` may be the best option to avoid
either evaluation errors or strange history entries. A new
`update_giotto_params()` call can then be added that describes the
function of the topmost function if desired.

## Usage

``` r
update_giotto_params(
  gobject,
  description = "_test",
  return_gobject = TRUE,
  toplevel = 2,
  attachments = NULL
)
```

## Arguments

- gobject:

  giotto object

- description:

  description of function run

- return_gobject:

  logical. Whether the giotto object should be returned

- toplevel:

  expected relative stackframe where call that is being recorded was
  made. If negative, param recording is skipped

- attachments:

  named list. Items to attach. These are intended for lightweight param
  classes containing settings. No large items should be added here.

## Value

giotto object or list of parameters

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

update_giotto_params(g, toplevel = 1)
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
