# Load image as a terra spatRaster object

Load image as a terra spatRaster object

## Usage

``` r
.create_terra_spatraster(image_path, page = 1L)
```

## Arguments

- image_path:

  existing full filepath to image to be loaded as a terra spatRaster

- page:

  integer. 1-based page (IFD) to read from a multi-page tif. Only
  consulted for formats that need the JPEG-2000 VRT route.

## Value

spatRaster object
