# Create a spatial weight matrix

Generate spatial weight matrix based on the strength of spatial
interactions between nodes. Requires spatial networks to be first
generated.

## Usage

``` r
createSpatialWeightMatrix(
  gobject,
  spat_unit = NULL,
  spatial_network_to_use = "kNN_network",
  method = c("distance", "adjacency"),
  wm_name = "spat_weights",
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- spatial_network_to_use:

  spatial network information to use

- method:

  type of weighted matrix to generate. See details

- wm_name:

  name to assign the weight matrix values

- return_gobject:

  (default = TRUE) whether to return as the giotto object with attached
  results or the bare weighted matrix

- verbose:

  be verbose

## Value

spatial weight matrix

## Details

- `"distance"` method is calculated using 1/(1+distance) to create an
  inverse weighting based on the distance between nodes.

- `"adjacency"` method is a binary matrix with 1 signifying that two
  nodes are connected in the spatial network and 0 indicating that they
  are not.

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

createSpatialWeightMatrix(g, spatial_network_to_use = "spatial_network")
#> Attaching weight matrix to spatial_network
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
