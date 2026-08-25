# Spatial and feature IDs

Get the cell/spot IDs (termed spatial IDs to better reflect when not at
the single-cell level) and feature IDs of a giotto object or subobject.

\[**`giotto` object specific**\] When applied on a `giotto` object,
these functions pull from the `cell_ID` and `feat_ID` slots. The values
within these slots are updated whenever the object is data is changed
and, importantly, whenever the active spat_unit and feat_type is set
(see
[`activeSpatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeSpatUnit-generic.md)
and
[`activeFeatType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/activeFeatType-generic.md)).
New values for these slots are specific to the active spat_unit and
feat_type and are detected from either the *subcellular* level
(`giottoPolygon` and `giottoPoints`) or the *aggregate* level
(expression matrix) data, with a preference for the latter if it exists.
Be aware that with this current behavior, values returned by`spatIDs()`
and `featIDs()` should be regarded as the minimal set of expected IDs
within all `giotto` slots, and not always the exact set or ordering.

## Usage

``` r
# S4 method for class 'giottoBinPoints'
featIDs(x, uniques = TRUE, ...)

# S4 method for class 'giotto'
spatIDs(x, spat_unit = NULL, subset, negate = FALSE, quote = TRUE, ...)

# S4 method for class 'exprObj'
spatIDs(x, ...)

# S4 method for class 'spatLocsObj'
spatIDs(x, ...)

# S4 method for class 'cellMetaObj'
spatIDs(x, ...)

# S4 method for class 'spatialNetworkObj'
spatIDs(x, ...)

# S4 method for class 'dimObj'
spatIDs(x, ...)

# S4 method for class 'giottoPolygon'
spatIDs(x, use_cache = TRUE, uniques = TRUE, ...)

# S4 method for class 'giottoPolygon'
spatIDs(x, old = NULL, ...) <- value

# S4 method for class 'spatEnrObj'
spatIDs(x, ...)

# S4 method for class 'nnNetObj'
spatIDs(x, ...)

# S4 method for class 'giotto'
featIDs(x, feat_type = NULL, subset, negate = FALSE, quote = TRUE, ...)

# S4 method for class 'exprObj'
featIDs(x, ...)

# S4 method for class 'featMetaObj'
featIDs(x, ...)

# S4 method for class 'giottoPoints'
featIDs(x, use_cache = TRUE, uniques = TRUE, ...)

# S4 method for class 'spatEnrObj'
featIDs(x, ...)
```

## Arguments

- x:

  an object

- uniques:

  return unique ID values only (currently gpoly and gpoints only)

- ...:

  additional params to pass when used with the `subset` param. For
  `spatID()`, these pass to
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md).
  For `featID()`, these currently only pass to
  [`fDataDT()`](https://giotto-suite.github.io/GiottoClass/dev/reference/fDataDT.md).

- spat_unit:

  (optional) specify which spatial unit

- subset:

  logical expression to find a subset of features.

- negate:

  logical. if `TRUE` all IDs that are **not** in the `subset` are
  selected

- quote:

  logical. If `TRUE`, the `subset` param will be quoted with
  [`substitute()`](https://rdrr.io/r/base/substitute.html). Set this to
  `FALSE` when calling from a function, although that may not be
  recommended since NSE output can be unexpected when not used
  interactively.

- use_cache:

  use cached IDs if available (gpoly and gpoints only)

- old:

  character. IDs to match against to replace

- value:

  character. IDs to replace with

- feat_type:

  (optional) specify which feature type

## Value

character vector of cell/spatial IDs or feature IDs

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
spatIDs(g)
spatIDs(g, subset = nr_feats <= 200)
spatIDs(g, subset = Dim.1 > 25, dim_reduction_to_use = "umap")

featIDs(g)
featIDs(g, subset = nr_cells < 100)

gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
featIDs(gpoints)

# ID replacements (currently only giottoPolygons)
polys <- g[["spatial_info"]][[1]]
slot(polys, "overlaps") <- NULL # make NULL to avoid a warning
head(spatIDs(polys))
spatIDs(polys) <- paste0("poly_", seq_len(nrow(polys)))
head(spatIDs(polys))
spatIDs(polys, old = c("poly_1", "poly_3")) <- c("test1", "test2")
head(spatIDs(polys))
```
