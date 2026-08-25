# addSpatialCentroidLocations

Calculates the centroid locations for the polygons within one or more
selected layers

## Usage

``` r
addSpatialCentroidLocations(
  gobject,
  poly_info = "cell",
  feat_type = NULL,
  spat_loc_name = "raw",
  provenance = poly_info,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  giotto object

- poly_info:

  polygon information

- feat_type:

  feature type

- spat_loc_name:

  name to give to the created spatial locations

- provenance:

  (optional) provenance to assign to generated spatLocsObj. If not
  provided, provenance will default to `poly_info`

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

## Value

If `return_gobject = TRUE` the giotto object containing the calculated
polygon centroids will be returned. If `return_gobject = FALSE` only the
generated polygon centroids will be returned as `spatLocObj`.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

addSpatialCentroidLocations(g, poly_info = "aggregate")
```
