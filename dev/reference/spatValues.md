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
  slot = NULL,
  svkey = NULL,
  view = NULL,
  space = NULL,
  samples = NULL,
  verbose = NULL,
  debug = FALSE
)

svkey(
  feats,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = NULL,
  spat_loc_name = NULL,
  spat_enr_name = NULL,
  poly_info = NULL,
  dim_reduction_to_use = NULL,
  dim_reduction_name = NULL,
  slot = NULL,
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

- slot:

  character. (optional) The `giotto` slot to search. Mainly
  `"cell_metadata"`: cell metadata has no name to pass, so this is the
  only way to scope the search to it. Use it when a metadata column
  could share a name with a feature, since an unscoped search checks
  expression first. The other slots are already scoped by passing their
  name param (`expression_values`, `spat_loc_name`, ...); `slot` also
  accepts `"expression"`, `"spatial_locs"`, `"spatial_enrichment"`,
  `"dimension_reduction"` and `"spatial_info"` (polygon info), which
  scope to that slot while leaving its item at the default. A `slot`
  that contradicts a name param is an error.

- svkey:

  a `svkey`, or a list of them. A single `svkey` is syntactic sugar for
  `svkey@get(gobject)`, and other params are ignored. A list pulls each
  key and joins the results on `cell_ID` (see details); `view`, `space`
  and `samples` apply to every key, and the other params are ignored.

- samples:

  character. (giottoMulti only) optional vector of sample names to
  narrow the joint output to. Joint slots (`@expression`,
  `@cell_metadata`) honor this directly; per-child-only slots (spatial
  locations, polygon info) remain inaccessible from
  `spatValues(mg, ...)` regardless — use the joint-level slots as the
  ground truth and avoid reaching into `mg[[sample]]` for per-child
  content.

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
param such as `expression_values`, `spat_enr_name`, etc, or a `slot` is
given, then the search will only be performed on that type of data.  
  
**\[svkey lists\]**  
Each key carries its own location through its params, so a list can pull
the same name from different slots, or from different items in one slot
(e.g. `raw` and `normalized` expression), in one call. Results are
joined on `cell_ID` as a full join: a cell missing from one key gets
`NA` there. All keys must resolve to the same `spat_unit`, since cell
IDs are not shared across units. When a value name comes back from more
than one key, those columns are prefixed with the key's list name, or,
for an unnamed list, with what locates the key (its `expression_values`,
`slot`, etc.). If that still does not tell them apart, name the list.  
  
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
spatValues(g, spat_unit = "aggregate", feats = "leiden_clus",
    slot = "cell_metadata"
)
```
