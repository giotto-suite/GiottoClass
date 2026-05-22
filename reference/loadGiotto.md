# loadGiotto

Saves a Giotto object to a specific folder structure

## Usage

``` r
loadGiotto(
  path_to_folder,
  load_params = list(),
  reconnect_giottoImage = TRUE,
  python_path = NULL,
  init_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- path_to_folder:

  path to folder where Giotto object was stored with
  [`saveGiotto`](https://giotto-suite.github.io/GiottoClass/reference/saveGiotto.md)

- load_params:

  additional parameters for loading or reading giotto object

- reconnect_giottoImage:

  (default = TRUE) whether to attempt reconnection of magick based image
  objects

- python_path:

  (optional) manually set your python path

- init_gobject:

  logical. Whether to initialize the `giotto` object after loading.
  (default = TRUE)

- verbose:

  be verbose

## Value

Giotto object

## Details

Works together with
[`saveGiotto`](https://giotto-suite.github.io/GiottoClass/reference/saveGiotto.md)
to save and re-load Giotto objects. Additional load_params need to be
provided as a list and will go to
[`readRDS`](https://rdrr.io/r/base/readRDS.html) or `qread` You can set
the python path, alternatively it will look for an existing Giotto
python environment.

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
td <- tempdir()
saveGiotto(gobject = g, dir = td)
#> 1. Start writing feature information
#> 2. Start writing spatial information
#> For spatial information: cell
#> 3. Start writing image information
#> For image information: alignment
#> For image information: image

loadGiotto(path_to_folder = paste0(td, "/saveGiottoDir"))
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
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
