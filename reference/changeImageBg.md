# changeImageBg

Function to change the background color of a magick image plot to
another color

## Usage

``` r
changeImageBg(
  mg_object,
  bg_color,
  perc_range = 10,
  new_color = "#FFFFFF",
  new_name = NULL
)
```

## Arguments

- mg_object:

  magick image or giotto image object

- bg_color:

  estimated current background color

- perc_range:

  range around estimated background color to include (percentage)

- new_color:

  new background color

- new_name:

  change name of Giotto image

## Value

magick image or giotto image object with updated background color

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
g_image <- convertGiottoLargeImageToMG(g,
    largeImage_name = "image",
    return_gobject = FALSE
)

changeImageBg(mg_object = g_image, bg_color = "white")
#> An object of class ' giottoImage ' with name  image 
#>  
#> Min and max values are: 
#>  Max on x-axis:  6790.5 
#>  Min on x-axis:  2000.5 
#>  Max on y-axis:  -2380.75 
#>  Min on y-axis:  -5730.25 
#>  
#> Boundary adjustment are: 
#>  Max adjustment on x-axis:  0 
#>  Min adjustment on x-axis:  0 
#>  Max adjustment on y-axis:  0 
#>  Min adjustment on y-axis:  0 
#>  
#> Boundaries are: 
#>  Image x-axis max boundary:  6790.5 
#>  Image x-axis min boundary:  2000.5 
#>  Image y-axis max boundary:  -2380.75 
#>  Image y-axis min boundary:  -5730.25 
#>  
#> Scale factor: 
#> x y 
#> 1 1 
#> 
#>  Resolution: 
#> x y 
#> 1 1 
#> 
#>  File Path: 
#> NULL
```
