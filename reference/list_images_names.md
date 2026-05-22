# list_images_names

return the available image names for a given image type that are
attached to the Giotto object

## Usage

``` r
list_images_names(gobject, img_type = NULL)
```

## Arguments

- gobject:

  a giotto object

- img_type:

  passing NULL (default) gets all image names. Can further specify
  "image" or "largeImage"

## Value

vector with names of available image names

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
list_images_names(g)
#> [1] "dapi_z0"  "dapi_z1"  "polyT_z0" "polyT_z1"
list_images_names(g, img_type = "largeImage")
#> [1] "dapi_z0"  "dapi_z1"  "polyT_z0" "polyT_z1"
```
