# Bins in polygons

The polygon each bin lies in, from the overlap of `x` with the bin
points `y` stored by
[`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md).
Works the same for in-memory and store-backed bin points.

## Usage

``` r
binOverlaps(x, y, name = NULL)
```

## Arguments

- x:

  `giottoPolygon` carrying the overlap

- y:

  the `giottoBinPoints` the overlap was calculated with

- name:

  name of the overlap (default: the feature type of `y`)

## Value

a `data.table` with `poly_ID` and `bin_ID`, one row per bin and polygon

## Examples

``` r
xy <- expand.grid(x = 0:3 + 0.5, y = 0:3 + 0.5)
ids <- paste0("bin", seq_len(nrow(xy)))
m <- Matrix::sparseMatrix(i = rep(1L, nrow(xy)), j = seq_len(nrow(xy)),
    x = 1, dimnames = list("g1", ids))
bp <- createGiottoBinPoints(createExprObj(m), createSpatLocsObj(
    data.frame(sdimx = xy$x, sdimy = xy$y, cell_ID = ids)))
gp <- createGiottoPolygon(data.frame(poly_ID = "a",
    x = c(0, 2, 2, 0), y = c(0, 0, 2, 2)))
binOverlaps(calculateOverlap(gp, bp), bp)
```
