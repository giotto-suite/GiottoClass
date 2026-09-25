# Giotto object history as NDJSON

Serialize
[`ghistory_records()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ghistory_records.md)
as newline-delimited JSON, one operation per line. Append-only by
construction: writing later steps never rewrites earlier lines.

## Usage

``` r
objHistory_ndjson(object, file = NULL)
```

## Arguments

- object:

  giotto object

- file:

  character. Optional path to write to. When `NULL` the text is
  returned.

## Value

character scalar, or the file path invisibly when `file` is given

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

cat(substr(objHistory_ndjson(g), 1, 120))
```
