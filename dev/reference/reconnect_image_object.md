# reconnect_image_object

Reconnect giotto image object with dead image pointer using a filepath
to the original image source

## Usage

``` r
reconnect_image_object(image_object, image_type, image_path)
```

## Arguments

- image_object:

  giotto image object

- image_type:

  type of giotto image object

- image_path:

  path to image source to reconnect image object with

## Value

reconnected image_object

## Details

This is a simple wrapper function for image object-specific reconnection
functions and does not include other functionality to find the specific
image objects in the giotto object.
