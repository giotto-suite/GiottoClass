# Polygon and Point Relationships

Utility class for storing overlaps relationships between polygons and
points in a sparse `data.table` format. `[i, ]` / `[, j]` / `[i, j]` all
return a subset `overlapPointDT`. To retrieve indices instead —
feat_ID_uniqs overlapped by poly i, or poly indices overlapping feature
j — pass `ids = TRUE`.

Supports `as.matrix` for conversion to `dgCMatrix`. Contained poly and
feature names simplify rownames/colnames and empty row/col creation.

## Usage

``` r
# S4 method for class 'overlapPointDT,gIndex,missing,missing'
x[i, j, ..., use_names = FALSE, ids = FALSE, drop]

# S4 method for class 'overlapPointDT,missing,gIndex,missing'
x[i, j, ..., use_names = FALSE, ids = FALSE, drop]

# S4 method for class 'overlapPointDT,gIndex,gIndex,missing'
x[i, j, ..., use_names = FALSE, drop]
```

## Arguments

- x:

  object

- i:

  numeric, character, logical. Index of or name of poly in overlapping
  polygons

- j:

  numeric, character, logical. Index of or name of feature being
  overlapped.

- ...:

  additional params to pass (none implemented)

- use_names:

  logical (default = `FALSE`). When `ids = TRUE`, whether to return
  integer indices (`FALSE`) or character ids (`TRUE`).

- ids:

  logical (default = `FALSE`). Whether to return the requested integer
  indices (`TRUE`) or the subset overlap object (`FALSE`, default).

- drop:

  not used.

## Value

A subset `overlapPointDT` by default. When `ids = TRUE`, an integer (or
character via `use_names = TRUE`) vector of the queried indices.

## Slots

- `data`:

  data.table. Table containing 3 integer cols:

  - `poly` - polygon index. Maps to `spat_ids` slot.

  - `feat` - feat_ID_uniq (unique integer identifier) of a point
    detection

  - `feat_id_index` - index of feature name mapping in `@feat_ids` slot.

- `spat_unit`:

  character. Spatial unit (usually name of polygons information)

- `feat_type`:

  character. Feature type (usually name of points information)

- `provenance`:

  character. provenance information

- `spat_ids`:

  character. Polygon names

- `feat_ids`:

  character. Feature names

- `nfeats`:

  integer (optional metadata). How many feature points were used in
  overlap operation. Gives an idea of sparsity, but has no effect on
  processing.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
poly <- g[["spatial_info", "z0"]][[1]]
ovlp <- overlaps(poly, "rna")
ovlp

as.matrix(ovlp)

dim(ovlp)
nrow(ovlp) # number of relationships

# subset (default) — returns an overlapPointDT
ovlp[1:10] # first 10 polys
ovlp[, 1:10] # first 10 feature species
ovlp[1:10, 1:10] # both

# selection query — feature unique IDs overlapped by nth poly
ovlp[1, ids = TRUE] # integer feat_ID_uniqs (integer(0) if no overlap)
ovlp[1:5, ids = TRUE]
ovlp[1:5, ids = TRUE, use_names = TRUE] # feature names instead of ints

# selection query — poly indices overlapping particular feature species
ovlp[, 1, ids = TRUE]
ovlp[, "Mlc1", ids = TRUE]
```
