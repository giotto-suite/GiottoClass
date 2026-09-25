# stitchTileCoordinates

Helper function to stitch tile coordinates together to form one complete
picture

## Usage

``` r
stitchTileCoordinates(location_file, Xtilespan, Ytilespan)
```

## Arguments

- location_file:

  location dataframe with X and Y coordinates

- Xtilespan:

  numerical value specifying the width of each tile

- Ytilespan:

  numerical value specifying the height of each tile

## Value

data.table

## Examples

``` r
location_file <- data.table::data.table(
    field = rep(c(1, 2), 5),
    X.X = rnorm(10), Y.Y = rnorm(10), XtileIndex = seq_len(10),
    YtileIndex = seq_len(10)
)

stitchTileCoordinates(location_file, Xtilespan = 0.5, Ytilespan = 0.5)
#>     field         X.X         Y.Y XtileIndex YtileIndex    Xcoord      Ycoord
#>     <num>       <num>       <num>      <int>      <int>     <num>       <num>
#>  1:     1  0.89885095  0.06035781          1          1 0.8988510  0.06035781
#>  2:     2 -0.09948527  1.79097845          2          2 0.4005147  2.29097845
#>  3:     1 -0.08511592 -1.01677778          3          3 0.9148841 -0.01677778
#>  4:     2 -0.64089585 -1.15285187          4          4 0.8591041  0.34714813
#>  5:     1 -1.88192649  0.07737059          5          5 0.1180735  2.07737059
#>  6:     2  1.08046622 -0.05951986          6          6 3.5804662  2.44048014
#>  7:     1  0.01222729  0.21080028          7          7 3.0122273  3.21080028
#>  8:     2 -1.33776388  1.11298916          8          8 2.1622361  4.61298916
#>  9:     1  2.52147936 -0.58110583          9          9 6.5214794  3.41889417
#> 10:     2 -0.80276253 -0.73998360         10         10 3.6972375  3.76001640
```
