# Sample values from SpatRaster

Sample numerical values from a `SpatRaster`. The output format depends
on the value of the `output` param.

## Usage

``` r
.spatraster_sample_values(
  raster_object,
  size = 5000,
  output = c("data.frame", "array", "magick", "EBImage", "SpatRaster"),
  verbose = NULL,
  ...
)
```

## Arguments

- raster_object:

  terra `SpatRaster` to sample from

- size:

  rough maximum of pixels allowed when resampling

- output:

  what output to return as. Defaults to "data.frame"

- verbose:

  be verbose

- ...:

  additional params to pass to
  [`terra::spatSample`](https://rspatial.github.io/terra/reference/sample.html)

## Value

magick or EBImage image
