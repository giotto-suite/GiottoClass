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

createSpatialWeightMatrix(g, spatial_network_to_use = "spatial_network")
```
