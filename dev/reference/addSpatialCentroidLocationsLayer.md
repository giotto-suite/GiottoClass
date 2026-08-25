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

addSpatialCentroidLocationsLayer(g, poly_info = "aggregate")
```
