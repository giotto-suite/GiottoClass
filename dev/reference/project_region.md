# Project a region between coordinate frames

Move a region drawn in one
[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)'s
frame into another's, through the native frame both are defined against.
A crop step records the frame its region was drawn in; this is what lets
it clip content returned in a different one.

## Usage

``` r
project_region(y, from_space = NULL, to_space = NULL)
```

## Arguments

- y:

  the region: a `SpatVector`, WKT `character`, `SpatExtent`, or a
  numeric `c(xmin, xmax, ymin, ymax)`

- from_space, to_space:

  [giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  objects scoped to one sample (or none), or `NULL` for the native frame

## Value

`y` in `to_space`'s frame. When both frames are native it is returned as
given, so a recorded WKT string stays WKT.
