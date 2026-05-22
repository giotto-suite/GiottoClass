# Calculate spatial network distance and weight

Calculate distance and weight metrics of a data.table spatial network

## Usage

``` r
.calculate_distance_and_weight(
  networkDT = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  sdimz = "sdimz",
  d2_or_d3 = c(2, 3)
)
```

## Arguments

- networkDT:

  spatial network as data.table

- sdimx:

  spatial dimension x

- sdimy:

  spatial dimension y

- sdimz:

  spatial dimension z

- d2_or_d3:

  number of dimensions

## Value

data.table
