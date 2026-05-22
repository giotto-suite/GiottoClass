# addSpatialCentroidLocationsLayer

Calculates the centroid locations for the polygons within one selected
layer

## Usage

``` r
addSpatialCentroidLocationsLayer(
  gobject,
  poly_info = "cell",
  feat_type = NULL,
  provenance = poly_info,
  spat_loc_name = "raw",
  return_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- poly_info:

  polygon information

- feat_type:

  feature type

- provenance:

  (optional) provenance to assign to generated spatLocsObj. If not
  provided, provenance will default to `poly_info`

- spat_loc_name:

  name to give to the created spatial locations

- return_gobject:

  return giotto object (default: TRUE)

## Value

If `return_gobject = TRUE` the giotto object containing the calculated
polygon centroids will be returned. If `return_gobject = FALSE` only the
generated polygon centroids will be returned as spatLocsObj.

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

addSpatialCentroidLocationsLayer(g, poly_info = "aggregate")
#> > spatial locations for polygon information layer " aggregate " and name " raw
#>  " already exists and will be replaced
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
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
