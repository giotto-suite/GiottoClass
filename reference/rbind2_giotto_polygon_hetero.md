# Append giotto polygons of different names

Append two giotto polygons together of different names Performed
recursively through `rbind2` and `rbind`. Generates an additional
list_ID attribute based on the original names. The object name also
becomes a combination of both previous names

## Usage

``` r
rbind2_giotto_polygon_hetero(x, y, new_name, add_list_ID = TRUE)
```

## Arguments

- x:

  `giottoPolygon` 1

- y:

  `giottoPolygon` 2

- add_list_ID:

  whether to include the name of the origin `giottoPolygons` as a new
  'list_ID' attribute

## Value

giottoPolygon
