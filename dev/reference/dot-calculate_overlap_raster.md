# Find feature points overlapped by rasterized polygon.

Core workflow function that accepts simple `SpatVector` inputs, performs
rasterization of the polys and then checks for overlaps.

## Usage

``` r
.calculate_overlap_raster(spatvec, pointvec, keep = NULL, verbose = TRUE)
```

## Arguments

- spatvec:

  `SpatVector` polygon from a `giottoPolygon` object

- pointvec:

  `SpatVector` points from a `giottoPoints` object

- keep:

  column(s) to keep

- verbose:

  be verbose

## Value

`SpatVector` of overlapped points info

## See also

[`calculateOverlapRaster()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlapRaster.md)
