# get_adj_rescale_img

get_adj_rescale_img

## Usage

``` r
get_adj_rescale_img(img_minmax, spatial_locs, scale_factor = 1)
```

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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g_spatlocs <- getSpatialLocations(g)
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")
minmax <- get_img_minmax(slot(mgimg, "mg_object"))
#> Warning: `get_img_minmax()` was deprecated in GiottoClass 0.3.1.
#> ℹ Please use `ext()` instead.

get_adj_rescale_img(img_minmax = minmax, spatial_locs = g_spatlocs)
#> Warning: `get_adj_rescale_img()` was deprecated in GiottoClass 0.3.1.
#> ℹ this is too specific to the inner workings of `giottoImage`
#> ℹ We can simply use `ext<-` to set a new extent instead of this.
#> xmin_adj_orig xmax_adj_orig ymin_adj_orig ymax_adj_orig 
#>          3069         -6198         -5270          2568 
```
