# Reconnect images with dead pointers

reconnect a gobject's dead image pointers using filepaths to the
original source image files

## Usage

``` r
reconnectGiottoImage(
  gobject,
  auto_reconnect = TRUE,
  reconnect_type = c("all", "image", "largeImage"),
  image_name = NULL,
  largeImage_name = NULL,
  image_path = NULL,
  largeImage_path = NULL,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- auto_reconnect:

  automatically reconnect images if TRUE. manual if FALSE

- reconnect_type:

  type of image to reconnect when auto_reconnect = TRUE

- image_name:

  names of images to reconnect

- largeImage_name:

  name of large images to reconnect

- image_path:

  named list of paths to images to reconnect to giottoImages

- largeImage_path:

  named list of paths to images to reconnect to giottoLargeImages

- verbose:

  be verbose

## Value

a giotto object with updated image pointer

## Details

Inputs can either be given as both image name
(`image_name`/`largeImage_name`) and filepath
(`image_path`/`largeImage_path`) args or as only a named list through a
filepath argument alone. If `auto_reconnect = TRUE` then no additional
params need to be supplied. As long as giotto image objects were
directly created using filepaths, those filepaths are stored within the
image objects and will be referenced during reconnection. Issues will
only arise if giotto image objects were created directly from the
underlying image handling package objects (*magick* or *raster objects*)
or if image files have been moved since the the giotto image object was
generated. In such cases, use manual reconnection by setting
`auto_reconnect = FALSE`.

## See also

Other basic image functions:
[`addGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/addGiottoImage.md),
[`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/plotGiottoImage.md),
[`updateGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/updateGiottoImage.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

reconnectGiottoImage(g, reconnect_type = "largeImage")
#> Attempting automatic reconnection...
#> largeImage(s) discovered...
#> --> alignment : filepath found
#> 
#> --> image : filepath found
#> 
#> 
#> done
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
