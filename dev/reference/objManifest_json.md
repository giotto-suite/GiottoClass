# Giotto object manifest as JSON

Serialize the manifest from
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md).
Keys are emitted in canonical (sorted) order and `NA`/`NaN`/`Inf` are
encoded as the strings `"NA"`, `"NaN"`, `"Inf"` and `"-Inf"` by schema
rule.

## Usage

``` r
objManifest_json(x, file = NULL, pretty = TRUE, ...)
```

## Arguments

- x:

  giotto object or a `gmanifest` from
  [`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)

- file:

  character. Optional path to write to. When `NULL` (default) the JSON
  is returned as a character scalar.

- pretty:

  logical. Whether to indent the output

- ...:

  additional params passed to
  [`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)
  when `x` is a `giotto` object

## Value

character scalar of JSON, or the file path, invisibly, when `file` is
given

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

cat(substr(objManifest_json(g), 1, 200))
```
