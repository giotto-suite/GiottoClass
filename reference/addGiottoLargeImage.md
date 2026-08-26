# addGiottoLargeImage

Add giotto image objects to `giotto` object

## Usage

``` r
addGiottoLargeImage(
  gobject = NULL,
  largeImages = NULL,
  spat_loc_name = NULL,
  scale_factor = NULL,
  negative_y = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- largeImages:

  list of giottoLargeImage objects

- spat_loc_name:

  provide spatial location slot in Giotto to align images. (optional)

- scale_factor:

  provide scale of image pixel dimensions relative to spatial
  coordinates.

- negative_y:

  map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

- verbose:

  be verbose

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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g_image <- getGiottoImage(g, image_type = "largeImage")

addGiottoLargeImage(g, largeImages = list(g_image))
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
