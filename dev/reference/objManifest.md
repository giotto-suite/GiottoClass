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

A manifest can also be read back without loading the object: pass the
folder written by
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md),
or the path to its `manifest.json`. The result is the manifest as it was
at save time, in the same form `objManifest()` returns for a live
object, so it can go to
[`manifestDiff()`](https://giotto-suite.github.io/GiottoClass/dev/reference/manifestDiff.md).

The JSON form follows the schema shipped in
`inst/schema/giotto-manifest-<version>.json`. Records are sorted by
their keys so two manifests of the same state serialize identically, and
`NA`, `NaN` and `Inf` are encoded as the strings `"NA"`, `"NaN"`,
`"Inf"` and `"-Inf"`.

## Usage

``` r
# S4 method for class 'gAny'
objManifest(
  x,
  levels = FALSE,
  output = c("gmanifest", "json"),
  file = NULL,
  pretty = TRUE,
  ...
)

# S4 method for class 'gmanifest'
objManifest(
  x,
  output = c("gmanifest", "json"),
  file = NULL,
  pretty = TRUE,
  ...
)

# S4 method for class 'character'
objManifest(
  x,
  output = c("gmanifest", "json"),
  file = NULL,
  pretty = TRUE,
  ...
)
```

## Arguments

- x:

  giotto object, a manifest from `objManifest()`, or the path to a
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  folder or its `manifest.json`. A disk-backed project directory is read
  through its source: pass `name` to pick a snapshot (the most recent by
  default).

- levels:

  logical. Whether to count the distinct values of each metadata column
  (`n_levels`), which shows a clustering result with its number of
  groups. Off by default: it is a full pass over every column, and the
  per-column fingerprints already detect the change. The id column is
  never counted. Not used when reading from a path.

- output:

  character. `"gmanifest"` (default) returns the manifest as an R list;
  `"json"` returns it as a JSON string.

- file:

  character. Optional path to write the manifest to as JSON. When given,
  the path is returned invisibly and `output` is ignored.

- pretty:

  logical. Whether to indent JSON output

- ...:

  additional params (none implemented)

## Value

list of class `gmanifest`, a JSON string, or the `file` path invisibly

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

m <- objManifest(g)
names(m$slots)

cat(substr(objManifest(g, output = "json"), 1, 200))
```
