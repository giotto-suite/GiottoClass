# Create terra spatvector object from a data.frame

create terra spatvector from a data.frame where cols 1 and 2 must be x
and y coordinates respectively. Additional columns are set as attributes
to the points where the first additional (col 3) should be the feat_ID.

## Usage

``` r
.create_spatvector_object_from_dfr(
  x,
  x_colname = NULL,
  y_colname = NULL,
  feat_ID_colname = NULL,
  verbose = NULL
)
```

## Arguments

- x:

  data.frame object

- x_colname:

  column name for x-coordinates

- y_colname:

  column name for y-coordinates

- feat_ID_colname:

  column name for feature ids

- verbose:

  be verbose

## Value

SpatVector
