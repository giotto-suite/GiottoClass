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
#>     field         X.X        Y.Y XtileIndex YtileIndex     Xcoord     Ycoord
#>     <num>       <num>      <num>      <int>      <int>      <num>      <num>
#>  1:     1 -1.15285187  0.2617988          1          1 -1.1528519  0.2617988
#>  2:     2  0.07737059 -0.1434333          2          2  0.5773706  0.3565667
#>  3:     1 -0.05951986 -1.2532630          3          3  0.9404801 -0.2532630
#>  4:     2  0.21080028  0.4716575          4          4  1.7108003  1.9716575
#>  5:     1  1.11298916 -1.0677673          5          5  3.1129892  0.9322327
#>  6:     2 -0.58110583 -1.2229453          6          6  1.9188942  1.2770547
#>  7:     1 -0.73998360  0.9307822          7          7  2.2600164  3.9307822
#>  8:     2 -2.96313621 -0.7520984          8          8  0.5368638  2.7479016
#>  9:     1 -0.83210706  1.0894913          9          9  3.1678929  5.0894913
#> 10:     2  1.17412658  0.4016715         10         10  5.6741266  4.9016715
```
