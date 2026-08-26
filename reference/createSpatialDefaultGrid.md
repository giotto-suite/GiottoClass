# createSpatialDefaultGrid

Create a spatial grid using the default method

## Usage

``` r
createSpatialDefaultGrid(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  sdimx_stepsize = NULL,
  sdimy_stepsize = NULL,
  sdimz_stepsize = NULL,
  minimum_padding = 1,
  name = NULL,
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  spatial location name

- sdimx_stepsize:

  stepsize along the x-axis

- sdimy_stepsize:

  stepsize along the y-axis

- sdimz_stepsize:

  stepsize along the z-axis

- minimum_padding:

  minimum padding on the edges

- name:

  name for spatial grid (default = 'spatial_grid')

- return_gobject:

  boolean: return giotto object (default = TRUE)

## Value

giotto object with updated spatial grid slot

## Details

Creates a spatial grid with defined x, y (and z) dimensions. The
dimension units are based on the provided spatial location units.

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

createSpatialDefaultGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
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
