# estimateImageBg

helps to estimate which color is the background color of your plot

## Usage

``` r
estimateImageBg(mg_object, top_color_range = seq_len(50))
```

## Arguments

- mg_object:

  magick image or Giotto image object

- top_color_range:

  top possible background colors to return

## Value

vector of pixel color frequencies and an associated barplot

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")
#> Error: package 'magick' is not yet installed
#> 
#>  To install:
#> install.packages(c("magick"))

estimateImageBg(mgimg)
#> Error: object 'mgimg' not found
```
