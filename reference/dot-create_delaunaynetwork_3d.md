# .create_delaunaynetwork_3d

Create a spatial 3D Delaunay network.

## Usage

``` r
.create_delaunaynetwork_3d(
  gobject,
  method = "delaunayn_geometry",
  spat_unit = NULL,
  spat_loc_name = "raw",
  sdimx = "sdimx",
  sdimy = "sdimy",
  sdimz = "sdimz",
  name = "delaunay_network_3D",
  maximum_distance = "auto",
  minimum_k = 0,
  options = "Pp",
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  ...
)
```

## Value

spatial 3D Delaunay network
