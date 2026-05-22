# rescalePolygons

Rescale individual polygons by a x and y factor

## Usage

``` r
rescalePolygons(
  gobject,
  poly_info = "cell",
  name = "rescaled_cell",
  fx = 0.5,
  fy = 0.5,
  calculate_centroids = TRUE,
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- poly_info:

  polygon information name

- name:

  name of new polygon layer

- fx:

  x-scaling factor

- fy:

  y-scaling factor

- calculate_centroids:

  calculate centroids

- return_gobject:

  return giotto object

## Value

giotto object

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

rescalePolygons(g, poly_info = "aggregate")
#> Setting polygon info [rescaled_cell]
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate rescaled_cell 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used
```
