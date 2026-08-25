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
```
