# addGiottoImage

Adds lists of giottoImages and giottoLargeImages to gobjects

## Usage

``` r
addGiottoImage(
  gobject = NULL,
  images = NULL,
  largeImages = NULL,
  spat_loc_name = NULL,
  scale_factor = NULL,
  negative_y = TRUE
)
```

## Arguments

- gobject:

  gobject to add images objects to

- images:

  list of giotto images to add

- largeImages:

  deprecated

- spat_loc_name:

  provide spatial location slot in Giotto to align giottoImages.
  Defaults to first one

- scale_factor:

  provide scale of image pixel dimensions relative to spatial
  coordinates.

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

## Value

an updated Giotto object with access to the list of images

## See also

Other basic image functions:
[`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/plotGiottoImage.md),
[`reconnectGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/reconnectGiottoImage.md),
[`updateGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/updateGiottoImage.md)

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

addGiottoImage(g, largeImages = list(g_image))
#> Warning: The `largeImages` argument of `addGiottoImage()` is deprecated as of
#> GiottoClass 0.3.0.
#> ℹ Please use the `images` argument instead.
#> ℹ All images should be supplied to `images` instead
#> ℹ Names of images may not overlap
#> 
#> alignment has already been used, will be overwritten
#> > image 'alignment' already exists and will be replaced
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
