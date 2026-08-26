# Zoom a Giotto spatial object

Works like
[`terra::zoom()`](https://rspatial.github.io/terra/reference/zoom.html).
Zoom in on a plot by providing a new extent, by default this is done by
clicking twice on the existing plot. When passing a `SpatExtent` without
drawing, no pre-existing plot is needed.

## Usage

``` r
# S4 method for class 'giottoLargeImage'
zoom(x, e = terra::draw(), ...)

# S4 method for class 'giottoPolygon'
zoom(x, e = terra::draw(), ...)

# S4 method for class 'giottoPoints'
zoom(x, e = terra::draw(), ...)
```

## Arguments

- x:

  giotto image, giottoPolygon, or giottoPoints object

- e:

  SpatExtent

- ...:

  additional parameters to pass to
  [`plot()`](https://giotto-suite.github.io/GiottoClass/reference/plot-generic.md)

## Value

SpatExtent (invisibly)

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
gimg <- getGiottoImage(g, image_type = "largeImage")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
e <- ext(6400, 6800, -4860, -4750) # arbitrary

# With extent passed
zoom(gimg, e)

zoom(gpoly, e)

zoom(gpoints, e)

```
