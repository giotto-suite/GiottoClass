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

rescalePolygons(g, poly_info = "aggregate")
```
