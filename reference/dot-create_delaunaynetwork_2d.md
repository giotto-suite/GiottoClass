# .create_delaunaynetwork_2d

Create a spatial 2D Delaunay network.

## Usage

``` r
.create_delaunaynetwork_2d(
  gobject,
  method = c("delaunayn_geometry", "RTriangle", "deldir"),
  spat_unit = NULL,
  spat_loc_name = "raw",
  sdimx = "sdimx",
  sdimy = "sdimy",
  name = "delaunay_network",
  maximum_distance = "auto",
  minimum_k = 0,
  options = "Pp",
  Y = TRUE,
  j = TRUE,
  S = 0,
  verbose = TRUE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  ...
)
```

## Value

spatial 2D Delaunay network
