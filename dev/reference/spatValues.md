# Giotto object spatial values

Retrieve specific values from the `giotto` object for a specific
`spat_unit` and `feat_type`. Values are returned as a data.table with
the features requested and a `cell_ID` column. This function may be
updated in the future to search in additional sets of information. To
see the currently available slot it checks, see details.

## Usage

``` r
spatValues(
  gobject,
  feats,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = NULL,
  spat_loc_name = NULL,
  spat_enr_name = NULL,
  poly_info = NULL,
  dim_reduction_to_use = NULL,
  dim_reduction_name = NULL,
  svkey = NULL,
  verbose = NULL,
  debug = FALSE
)

svkey(
  feats,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = NULL,
  spat_loc_name = NULL,
  poly_info = NULL,
  dim_reduction_to_use = NULL,
  dim_reduction_name = NULL,
  verbose = NULL
)
```

## Arguments

- gobject:

  `giotto` object

- feats:

  character vector. One or more features or values to find within the
  giotto object

- spat_unit:

  character. spatial unit to check

- feat_type:

  character. feature type to check

- expression_values:

  character. (optional) Name of expression information to use

- spat_loc_name:

  character. (optional) Name of spatial locations information to use

- spat_enr_name:

  character. (optional) Name of spatial enrichments to use

- poly_info:

  character. (optional) Name of polygons to use

- dim_reduction_to_use:

  character. (optional) Which type of dimension reduction to use

- dim_reduction_name:

  character. (optional) Name of dimension reduction to use

- svkey:

  use a `svkey`. Other params will be ignored. This is just syntactic
  sugar for `svkey@get(gobject)`

- verbose:

  verbosity

- debug:

  logical. (default = FALSE) See details.

## Value

A data.table with a cell_ID column and whichever feats were requested

## Details

**\[search\]**  
spatValues searches through the set of available information within the
`giotto` object for matches to `feats`. The current search order is

1.  cell expression

2.  cell metadata

3.  spatial locations

4.  spatial enrichment

5.  dimension reduction

6.  polygon info

If a specific name for one of the types of information is provided via a
param such as `expression_values`, `spat_enr_name`, etc, then the search
will only be performed on that type of data.  
  
**\[debug\]**  
This function uses Giotto's accessor functions which can usually throw
errors whenever a specific set of data or the features within that set
do not exist. This function muffles those errors, and only sends an
error that the data was not found when all getters fail. By setting
`debug = TRUE`, you can see the errors returned from each failed getter
printed as messages for easier debugging.

## Functions

- `svkey()`: Create a `svkey` defining a `spatValues()` call for
  deferred use. Handy for contexts where the full `giotto` object is not
  available.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")

# expression
spatValues(g, spat_unit = "aggregate", feats = c("Mlc1", "Gfap"))
spatValues(g,
    spat_unit = "aggregate", feats = c("Mlc1", "Gfap"),
    expression_values = "normalized"
)

# spatial enrichment
spatValues(g, spat_unit = "aggregate", feats = c("1", "3"))

# polygon info
spatValues(g, spat_unit = "aggregate", feats = c("agg_n", "valid"))

# cell meta
spatValues(g, spat_unit = "aggregate", feats = c("nr_feats"))
```
