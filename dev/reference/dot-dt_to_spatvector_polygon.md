# Convert data.table to polygon spatVector

convert data.table to spatVector for polygons

## Usage

``` r
.dt_to_spatvector_polygon(
  dt,
  include_values = TRUE,
  specific_values = NULL,
  sort_geom = FALSE
)
```

## Arguments

- dt:

  `data.table`. terra geometry information

- include_values:

  `logical`. Whether to include additional columns other than the
  geometry information as `SpatVector` attributes. Default is TRUE.

- specific_values:

  `character`. Specific subset of columns to include as attributes if
  `include_values = TRUE`.

- sort_geom:

  `logical`. Whether to sort key the data.table input by 'geom', 'part',
  and 'hole' columns.

## Value

polygon spatVector
