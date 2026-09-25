# Record a giotto object history step

Append a history entry that
[`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md)
cannot produce itself: a call that failed, or a change made outside a
logging function.

Only 52 of the suite's functions call
[`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md),
so a manifest can move without any entry claiming it. An execution tool
that diffs
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)
before and after a chunk closes that gap by recording the unclaimed
change here with `status = "unattributed"`, rather than letting it
vanish.

## Usage

``` r
recordGiottoStep(
  gobject,
  fn = NA_character_,
  params = list(),
  status = c("ok", "error", "unattributed"),
  diff = NULL,
  error = NULL,
  description = NULL
)
```

## Arguments

- gobject:

  giotto object

- fn:

  character. Name of the function or code that ran

- params:

  list. Parameters, as deparsed strings

- status:

  character. One of "ok", "error", "unattributed"

- diff:

  list. Manifest delta from
  [`manifestDiff()`](https://giotto-suite.github.io/GiottoClass/dev/reference/manifestDiff.md)

- error:

  character. Error message, when `status = "error"`

- description:

  character. Suffix for the step name

## Value

giotto object

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

g <- recordGiottoStep(g, fn = "manual edit", status = "unattributed")
tail(names(objHistory(g)), 1)
```
