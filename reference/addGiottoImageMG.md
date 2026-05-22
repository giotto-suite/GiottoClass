# addGiottoImageMG

Adds giotto image objects to your giotto object

## Usage

``` r
addGiottoImageMG(
  gobject,
  images,
  spat_unit = NULL,
  spat_loc_name = NULL,
  scale_factor = NULL,
  negative_y = TRUE
)
```

## Arguments

- gobject:

  giotto object

- images:

  list of giotto image objects, see
  [`createGiottoImage`](https://giotto-suite.github.io/GiottoClass/reference/createGiottoImage.md)

- spat_unit:

  spatial unit

- spat_loc_name:

  provide spatial location slot in Giotto to align images. Defaults to
  first one

- scale_factor:

  provide scale of image pixel dimensions relative to spatial
  coordinates.

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

## Value

an updated Giotto object with access to the list of images

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
g_image <- getGiottoImage(g, image_type = "largeImage")

addGiottoImageMG(g, images = list(g_image))
#> Warning: image [1] is not a giotto image object
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
