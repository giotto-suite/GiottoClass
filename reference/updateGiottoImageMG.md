# updateGiottoImageMG

Updates the boundaries of a giotto `image` object attached to a `giotto`
object if both `gobject` and `image_name` params are given.
Alternatively can directly accept and return as `image`

## Usage

``` r
updateGiottoImageMG(
  gobject = NULL,
  image_name = NULL,
  giottoImage = NULL,
  xmax_adj = 0,
  xmin_adj = 0,
  ymax_adj = 0,
  ymin_adj = 0,
  x_shift = 0,
  y_shift = 0,
  scale_factor = NULL,
  scale_x = 1,
  scale_y = 1,
  order = c("first_adj", "first_scale"),
  xmin_set = NULL,
  xmax_set = NULL,
  ymin_set = NULL,
  ymax_set = NULL,
  return_gobject = TRUE,
  verbose = TRUE
)
```

## Arguments

- gobject:

  `giotto` object containing giotto `image` object

- image_name:

  name of giotto `image` object

- giottoImage:

  `image` object to directly update

- xmax_adj, xmin_adj, ymax_adj, ymin_adj:

  adjust image boundaries by increasing maximum and decreasing minimum
  bounds respectively of xy bounds

- x_shift, y_shift:

  shift entire image along xy axes

- scale_factor:

  set `scale_x` and `scale_y` params at the same time

- scale_x, scale_y:

  independently scale x or y axis image mapping from coordinate origin

- order:

  order of operations between fine adjustments (adjustment and shift
  parameters) and scaling

- xmin_set:

  set image xmin boundary. Applied before adjustments

- xmax_set:

  set image xmax boundary. Applied before adjustments

- ymin_set:

  set image ymin boundary. Applied before adjustments

- ymax_set:

  set image ymax boundary. Applied before adjustments

- return_gobject:

  return a `giotto` object if `TRUE`, a giotto `image` object if `FALSE`

- verbose:

  be verbose

## Value

a `giotto` object or an updated giotto `image` object if
`return_gobject = FALSe`

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

updateGiottoImageMG(g, giottoImage = g_image)
#> giottoImage argument is given and will take priority
#> 
#>  return_gobject set to FALSE
#> Error in updateGiottoImageMG(g, giottoImage = g_image): object 'g_image' not found
```
