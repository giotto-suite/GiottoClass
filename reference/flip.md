# Flip an object

Flip an object over a designated x or y value depending on direction
param input. Note that this behavior may be different from terra's

## Usage

``` r
# S4 method for class 'giotto'
flip(
  x,
  direction = "vertical",
  x0 = 0,
  y0 = 0,
  spat_unit = ":all:",
  feat_type = ":all:",
  ...
)

# S4 method for class 'giottoPolygon'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'giottoPoints'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'spatLocsObj'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'spatialNetworkObj'
flip(x, direction = "vertical", x0 = 0, y0 = 0, ...)

# S4 method for class 'giottoLargeImage'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'SpatExtent'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'giottoAffineImage'
flip(x, direction = "vertical", x0 = 0, y0 = 0)

# S4 method for class 'affine2d'
flip(x, direction = "vertical", x0 = 0, y0 = 0)
```

## Arguments

- x:

  object

- direction:

  character. Direction to flip. Should be either partial match to
  'vertical' or 'horizontal'

- x0:

  x value to flip horizontally over (ignored for vertical). Pass NULL to
  flip over the extent

- y0:

  y value to flip vertically over (ignored for horizontal). Pass NULL to
  flip over the extent

- spat_unit:

  character vector. spatial units to affect

- feat_type:

  character vector. feature types to affect

- ...:

  additional args to pass

## Value

flipped object

## Examples

``` r
g <- GiottoData::loadSubObjectMini("spatLocsObj")

flip(g)
#> An object of class spatLocsObj : "raw"
#> spat_unit : "aggregate"
#> provenance: z0 z1 
#> dimensions: 462 3 
#> preview   :
#>       sdimx    sdimy                                 cell_ID
#>       <num>    <num>                                  <char>
#> 1: 6637.881 5140.465 100210519278873141813371229408401071444
#> 2: 6471.978 4883.541 101161259912191124732236989250178928032
#> 3: 6801.610 4968.685 101488859781016188084173008420811094152
#> 
#> ranges:
#>         sdimx    sdimy
#> [1,] 6401.412 4700.326
#> [2,] 6899.108 5146.747
#> 
```
