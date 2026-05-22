# get_img_minmax

get_img_minmax

## Usage

``` r
get_img_minmax(mg_img, negative_y = TRUE)
```

## Arguments

- mg_img:

  magick object

- negative_y:

  Map image to negative y spatial values if TRUE during automatic
  alignment. Meaning that origin is in upper left instead of lower left.

## Value

numeric

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
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")

get_img_minmax(slot(mgimg, "mg_object"))
#> $img_xmax
#> [1] 243
#> 
#> $img_xmin
#> [1] 0
#> 
#> $img_ymax
#> [1] 0
#> 
#> $img_ymin
#> [1] -172
#> 
```
