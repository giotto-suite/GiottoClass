# updateGiottoLargeImage

Updates the boundaries of a giotto `largeImage` object attached to a
`giotto` object if both `gobject` and `largeImage_name` params are
given. Alternatively can directly accept and return as `largeImage`

## Usage

``` r
updateGiottoLargeImage(
  gobject = NULL,
  largeImage_name = NULL,
  giottoLargeImage = NULL,
  xmax_adj = 0,
  xmin_adj = 0,
  ymax_adj = 0,
  ymin_adj = 0,
  x_shift = 0,
  y_shift = 0,
  scale_factor = NULL,
  scale_x = 1,
  scale_y = 1,
  order = c("first_adj", "first_scale"),
  xmin_set = NULL,
  xmax_set = NULL,
  ymin_set = NULL,
  ymax_set = NULL,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  `giotto` object containing giotto `largeImage` object

- largeImage_name:

  name of giotto `largeImage` object

- giottoLargeImage:

  `largeImage` object to directly update

- xmax_adj, xmin_adj, ymax_adj, ymin_adj:

  adjust image boundaries by increasing maximum and decreasing minimum
  bounds respectively of xy bounds

- x_shift, y_shift:

  shift entire image along xy axes

- scale_factor:

  set `scale_x` and `scale_y` params at the same time

- scale_x, scale_y:

  independently scale x or y axis image mapping from coordinate origin

- order:

  order of operations between fine adjustments (adjustment and shift
  parameters) and scaling

- xmin_set, xmax_set, ymin_set, ymax_set:

  directly set xy image boundaries. Overrides minmax values as spatial
  anchor.

- return_gobject:

  return a `giotto` object if `TRUE`, a giotto `largeImage` object if
  `FALSE`

- verbose:

  be verbose

## Value

a `giotto` object or an updated giotto `largeImage` object if
`return_gobject = FALSE`

## See also

[`ext()`](https://giotto-suite.github.io/GiottoClass/reference/ext.md)

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

updateGiottoLargeImage(g, largeImage_name = "image")
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
