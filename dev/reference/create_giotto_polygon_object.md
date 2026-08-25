# Create a giotto polygon object

Create a giotto polygon object

## Usage

``` r
create_giotto_polygon_object(
  name = "cell",
  spatVector = NULL,
  spatVectorCentroids = NULL,
  overlaps = NULL,
  unique_IDs = NULL
)
```

## Arguments

- name:

  name of polygon object

- spatVector:

  SpatVector of polygons

- spatVectorCentroids:

  (optional) SpatVector of polygon centroids

- overlaps:

  (optional) feature overlaps of polygons

- unique_IDs:

  unique polygon IDs for cacheing

## Value

giotto_polygon_object
