# convertGiottoLargeImageToMG

convert a giottoLargeImage by downsampling into a normal magick based
giottoImage

## Usage

``` r
convertGiottoLargeImageToMG(
  gobject = NULL,
  largeImage_name = NULL,
  giottoLargeImage = NULL,
  mg_name = NULL,
  spat_unit = NULL,
  spat_loc_name = NULL,
  crop_extent = NULL,
  xmax_crop = NULL,
  xmin_crop = NULL,
  ymax_crop = NULL,
  ymin_crop = NULL,
  resample_size = 5e+05,
  max_intensity = NULL,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  gobject containing giottoLargeImage

- largeImage_name:

  name of giottoLargeImage

- giottoLargeImage:

  alternative input param using giottoLargeImage object instead of
  through `gobject` and `largeImage_name` params

- mg_name:

  name to assign converted magick image based giottoImage. Defaults to
  name of giottoLargeImage

- spat_unit:

  spatial unit

- spat_loc_name:

  gobject spatial location name to map giottoImage to (optional)

- crop_extent:

  extent object to focus on specific region of image

- xmax_crop:

  assign crop boundary

- xmin_crop:

  assign crop boundary

- ymax_crop:

  assign crop boundary

- ymin_crop:

  assign crop boundary

- resample_size:

  maximum number of pixels to use when resampling

- max_intensity:

  value to treat as maximum intensity in color scale

- return_gobject:

  return as giotto object

- verbose:

  be verbose

## Value

a giotto object if `return_gobject = TRUE` or an updated giotto image
object if `return_gobject = FALSE`

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

convertGiottoLargeImageToMG(g, largeImage_name = "image")
#> image has already been used, will be overwritten
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
