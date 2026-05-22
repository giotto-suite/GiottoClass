# S4 giottoImage Class

Framework of giotto object to store and work with spatial expression
data

## Value

giottoImage

## Details

\[**mg_object**\] Core object is any image that can be read by the
magick package

\[**boundaries**\] Boundary adjustments can be used to manually or
automatically through a script adjust the image with the spatial data.

## Slots

- `name`:

  name of Giotto image

- `mg_object`:

  magick image object

- `minmax`:

  minimum and maximum of associated spatial location coordinates

- `boundaries`:

  x and y coordinate adjustments (default to 0)

- `scale_factor`:

  image scaling relative to spatial locations

- `resolution`:

  spatial location units covered per pixel

- `file_path`:

  file path to the image if given

- `OS_platform`:

  Operating System to run Giotto analysis on

## Examples

``` r
giottoImage()
#> An object of class ' giottoImage ' with name  test 
#>  
#> Min and max values are: 
#>  Max on x-axis:  
#>  Min on x-axis:  
#>  Max on y-axis:  
#>  Min on y-axis:  
#>  
#> Boundary adjustment are: 
#>  Max adjustment on x-axis:  
#>  Min adjustment on x-axis:  
#>  Max adjustment on y-axis:  
#>  Min adjustment on y-axis:  
#>  
#> Boundaries are: 
#>  Image x-axis max boundary:   
#>  Image x-axis min boundary:   
#>  Image y-axis max boundary:   
#>  Image y-axis min boundary:   
#>  
#> Scale factor: 
#> NULL
#> 
#>  Resolution: 
#> NULL
#> 
#>  File Path: 
#> NULL
```
