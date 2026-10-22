# Giotto object manifest

Machine-readable inventory of a `giotto` object's contents: identity, a
summary block, and one record per subobject grouped by slot. Each record
carries the keys that retrieve it under `keys` (`spat_unit`,
`feat_type`, `name`, and `reduction` / `reduction_method` / `nn_type`
where they apply), named as the accessor arguments. Derived on demand,
so it is always current.

Companion to
[`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md),
which records *why* an object looks the way it does. The manifest
records *what it is*. Object state is never reconstructed by replaying
history.

Every accessor used is individually guarded: a field that cannot be read
becomes `NULL` and its path is listed in the `warnings` element, rather
than erroring the manifest. Every record carries a content fingerprint
from
[`fingerprint_data()`](https://giotto-suite.github.io/GiottoClass/dev/reference/fingerprint_data.md).
That is what detects a step that overwrites a matrix or a column in
place, which leaves every name unchanged. In-memory content is hashed in
full; disk-backed payloads are identified from their handle; images from
their pixels at display resolution. Geometry is stride-sampled, with
coordinates compared to 12 significant digits so a shapefile round trip
does not read as a change. Sizes and extents are not reported: on a
disk-backed object they cost a scan.

## Usage

``` r
# S4 method for class 'giotto'
objManifest(x, levels = FALSE, ...)
```

## Arguments

- x:

  giotto object

- levels:

  logical. Whether to count the distinct values of each metadata column
  (`n_levels`), which shows a clustering result with its number of
  groups. Off by default: it is a full pass over every column, and the
  per-column fingerprints already detect the change. The id column is
  never counted.

- ...:

  additional params (none implemented)

## Value

list of class `gmanifest`

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

m <- objManifest(g)
names(m$slots)
```
