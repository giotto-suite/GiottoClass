# .terra_writeraster_datatype

find likely compatible datatype for given image characteristics. Values
given in arguments take priority over those found from giottoLargeImage
metadata

## Usage

``` r
.terra_writeraster_datatype(
  giottoLargeImage = NULL,
  quick_INTS_maxval = NULL,
  max_intensity = NULL,
  min_intensity = NULL,
  is_int = NULL,
  signed = NULL,
  bitDepth = NULL,
  verbose = TRUE
)
```

## Arguments

- giottoLargeImage:

  giottoLargeImage object to determine max_intensity, min_intensity,
  is_int settings from

- quick_INTS_maxval:

  Treat as maximum intensity to find compatible unsigned integer
  settings

- max_intensity, min_intensity:

  value given as image maximum/minimum intensity

- is_int:

  if image is integer (TRUE) or floating point (FALSE)

- signed:

  if image is signed (TRUE) or unsigned (TRUE)

- bitDepth:

  image bitDepth

- verbose:

  be verbose

## Value

datatype for terra writeRaster function
