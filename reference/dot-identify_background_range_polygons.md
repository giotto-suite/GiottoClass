# Identify background range polygons

function to remove background polygon by comparing individual polygon
vertex xy ranges against the overall `SpatVector` extent. Each
`SpatVector` that this function is run on MUST contain only those
polygons that were generated as part of the same FOV. This is because
the individual polygons are compared against the overall extent which is
assumed to be roughly that of the original mask image.

## Usage

``` r
.identify_background_range_polygons(spatVector, threshold = 0.9)
```

## Arguments

- spatVector:

  terra polygon spatvector

- threshold:

  if a polygon's x AND y range exceeds this fraction of the overall
  `SpatVector` extent, it will be flagged for removal.

## Value

A character vector of polygon IDs of variable length (but usually only
either length 0 i.e. no background poly to remove, or 1, being the
background poly to remove) if used with a correct threshold.
