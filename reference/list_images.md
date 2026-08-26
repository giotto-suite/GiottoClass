# list_images

Prints the available giotto images that are attached to the Giotto
object

## Usage

``` r
list_images(gobject, img_type = NULL)
```

## Arguments

- gobject:

  giotto object

- img_type:

  "image" or "largeImage"

## Value

data.table of giotto image names attached to the giotto object

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
list_images(g)
#>        name   img_type
#>      <char>     <char>
#> 1:  dapi_z0 largeImage
#> 2:  dapi_z1 largeImage
#> 3: polyT_z0 largeImage
#> 4: polyT_z1 largeImage
list_images(g, img_type = "largeImage")
#>        name   img_type
#>      <char>     <char>
#> 1:  dapi_z0 largeImage
#> 2:  dapi_z1 largeImage
#> 3: polyT_z0 largeImage
#> 4: polyT_z1 largeImage
```
