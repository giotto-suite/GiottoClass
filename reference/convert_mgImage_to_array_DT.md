# convert_mgImage_to_array_DT

converts a magick image object to a data.table

## Usage

``` r
convert_mgImage_to_array_DT(mg_object)
```

## Arguments

- mg_object:

  magick image or Giotto image object

## Value

data.table with image pixel information

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

a <- convert_mgImage_to_array_DT(mgimg)
#> Error: object 'mgimg' not found
force(a)
#> Error: object 'a' not found
force(a)
#> Error: object 'a' not found
```
