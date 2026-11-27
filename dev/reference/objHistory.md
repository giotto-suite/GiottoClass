# Giotto object history

The steps recorded on a giotto object: one record per step, holding the
function (`fn`), the parameters it ran with as deparsed expressions
(`params`), and `timestamp`, `seed`, `status`, `error` and `diff`. Steps
recorded before these records existed are reported with `status = "ok"`
and the parameters they recorded.

The history can also be read back without loading the object: pass the
folder written by
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md),
or the path to its `history.ndjson`. The result is the same as
`objHistory()` on the object at save time.

The history is provenance, not state: only functions that opt in are
recorded. What the object holds comes from
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md).

## Usage

``` r
# S4 method for class 'gAny'
objHistory(
  x,
  summarized = FALSE,
  output = c("ghistory", "ndjson"),
  file = NULL,
  ...
)

# S4 method for class 'ghistory'
objHistory(
  x,
  summarized = FALSE,
  output = c("ghistory", "ndjson"),
  file = NULL,
  ...
)

# S4 method for class 'character'
objHistory(
  x,
  summarized = FALSE,
  output = c("ghistory", "ndjson"),
  file = NULL,
  ...
)
```

## Arguments

- x:

  giotto object, a history from `objHistory()`, or the path to a
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  folder or its `history.ndjson`. A disk-backed project directory is
  read through its source: pass `name` to pick a snapshot (the most
  recent by default).

- summarized:

  logical. Whether to also print a summary of the steps and the names
  they used

- output:

  character. `"ghistory"` (default) returns the records as a list;
  `"ndjson"` returns them as newline-delimited JSON, one step per line.

- file:

  character. Optional path to write the history to as NDJSON. When
  given, the path is returned invisibly and `output` is ignored.

- ...:

  additional params (none implemented)

## Value

list of class `ghistory`, an NDJSON string, or the `file` path invisibly

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

objHistory(g)
objHistory(g, summarized = TRUE)
cat(substr(objHistory(g, output = "ndjson"), 1, 120))
```
