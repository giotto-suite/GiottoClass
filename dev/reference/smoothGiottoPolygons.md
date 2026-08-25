# smoothGiottoPolygons

Smooths Giotto polygon object

## Usage

``` r
smoothGiottoPolygons(
  gpolygon,
  vertices = 20,
  k = 3,
  set_neg_to_zero = TRUE,
  ...
)
```

## Arguments

- gpolygon:

  giotto polygon object

- vertices:

  number of vertices

- k:

  k

- set_neg_to_zero:

  set negative values to zero (default: TRUE)

- ...:

  additional params to pass to `spline`

## Value

Smoothed Giotto polygon object with reduced vertices

## See also

[`spline`](https://rdrr.io/r/stats/splinefun.html)

## Examples

``` r
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")

smoothGiottoPolygons(gpolygon = gpoly)
```
