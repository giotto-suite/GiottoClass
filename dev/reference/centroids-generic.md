# centroids-generic

Access centroids information from polygon objects

## Usage

``` r
# S4 method for class 'giottoPolygon'
centroids(x, append_gpolygon = FALSE)
```

## Arguments

- x:

  object

- append_gpolygon:

  whether to append the centroids results to the `giottoPolygon` instead
  of returning bare `SpatVector`. Defaults to FALSE

## Value

spatVectorCentroids or spatVector

## Details

For giottoPolygon, if centroids already exist, pulls from
`spatVectorCentroids` slot. Otherwise, generates from `spatVector` slot
de novo

## Examples

``` r
g <- GiottoData::loadSubObjectMini("giottoPolygon")

centroids(g)
```
