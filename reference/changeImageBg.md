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
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g_image <- convertGiottoLargeImageToMG(g,
    largeImage_name = "image",
    return_gobject = FALSE
)
#> Error in loadNamespace(x): there is no package called ‘magick’

changeImageBg(mg_object = g_image, bg_color = "white")
#> Error: object 'g_image' not found
```
