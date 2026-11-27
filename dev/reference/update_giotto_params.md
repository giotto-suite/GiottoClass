# Update giotto parameters

For developer use. Adds an entry to the `giotto` object object history.
Care currently needs to be taken when a function that contains a call to
this function is called from within yet another function. In such cases,
a `toplevel < 0` or setting a temporary `"giotto.update_param" = FALSE`
with `GiottoUtils::gwith_option()` may be the best option to avoid
either evaluation errors or strange history entries. A new
`update_giotto_params()` call can then be added that describes the
function of the topmost function if desired.

## Usage

``` r
update_giotto_params(
  gobject,
  description = "_test",
  return_gobject = TRUE,
  toplevel = 2,
  attachments = NULL
)
```

## Arguments

- gobject:

  giotto object

- description:

  description of function run

- return_gobject:

  logical. Whether the giotto object should be returned

- toplevel:

  expected relative stackframe where call that is being recorded was
  made. If negative, param recording is skipped

- attachments:

  named list. Items to attach. These are intended for lightweight param
  classes containing settings. No large items should be added here. A
  param with a
  [`methodCitation()`](https://giotto-suite.github.io/GiottoClass/dev/reference/methodCitation.md)
  (or a `bibentry`) also records its citation with the step, shown once
  per session and listed by
  [`objCitations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objCitations.md).
  An item `py_versions` (the versions a Python tool ran with, from
  [`GiottoUtils::py_run()`](https://giotto-suite.github.io/GiottoUtils/reference/py_run.html))
  is recorded with the step, so it is part of
  [`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md)
  and survives
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md).

## Value

giotto object or list of parameters

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

update_giotto_params(g, toplevel = 1)
```
