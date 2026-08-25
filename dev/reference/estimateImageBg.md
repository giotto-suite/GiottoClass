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
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")

estimateImageBg(mgimg)
```
