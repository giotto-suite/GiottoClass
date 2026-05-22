# Convert point data data.table to spatVector

data.table to spatVector for points

## Usage

``` r
.dt_to_spatvector_points(dt, include_values = TRUE, specific_values = NULL)
```

## Arguments

- dt:

  data.table

- include_values:

  boolean. Include additional values from data.table as attributes
  paired with created terra spatVector

- specific_values:

  specific values to include as attributes if include_values == TRUE

## Value

spatVector for points
