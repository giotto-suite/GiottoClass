# Giotto object manifest

Machine-readable inventory of a `giotto` object's contents: identity, a
summary block, and a slot-by-slot description nested the same way the
object is (`spat_unit` x `feat_type` x name). Derived on demand, so it
is always current.

Companion to
[`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md),
which records *why* an object looks the way it does. The manifest
records *what it is*. Object state is never reconstructed by replaying
history.

Every accessor used is individually guarded: a field that cannot be read
becomes `NULL` and its path is listed in the `warnings` element, rather
than erroring the manifest.

## Usage

``` r
# S4 method for class 'giotto'
objManifest(x, level = c("summary", "full"), fingerprint = NULL, ...)
```

## Arguments

- x:

  giotto object

- level:

  character. `"summary"` (default) omits fingerprints. `"full"` adds
  them, which is what detects an operation that overwrites a matrix or a
  column in place - a change `"summary"` cannot see, since the shape and
  the names are identical on both sides. Sampled fingerprints cost
  little: they read a fixed number of values regardless of object size.

- fingerprint:

  character. `"none"`, `"sample"` (hash of a deterministic fixed-stride
  slice of the content) or `"full"` (hash of all content). Defaults to
  `"none"` for `level = "summary"` and `"sample"` for `level = "full"`.
  Overrides `level` when given.

  Numbers are compared to 12 significant digits, so a fingerprint
  identifies values rather than bit patterns. Re-reading a saved object
  does not report a change when a storage format has dropped the last
  bits of a double, and by the same token a difference below roughly
  1e-12 relative is not reported at all.

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
