# S4 giotto polygon Class

Giotto class to store and operate on polygon-like data

## Value

giottoPolygon

## Details

holds polygon data

## Slots

- `name`:

  name of polygon shapes

- `spatVector`:

  terra spatVector to store polygon shapes

- `spatVectorCentroids`:

  centroids of polygon shapes

- `overlaps`:

  information about overlapping points and polygons

- `unique_ID_cache`:

  cached unique spatial IDs that should match the spatVector slot

## Examples

``` r
giottoPolygon()
```
