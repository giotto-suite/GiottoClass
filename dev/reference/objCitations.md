# Citations of the methods used on an object

The publications to cite for the steps recorded in an object's history
([`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md)):
one entry per method, in the order first used. Only steps whose method
declares a
[`methodCitation()`](https://giotto-suite.github.io/GiottoClass/dev/reference/methodCitation.md)
are listed.

## Usage

``` r
objCitations(x)
```

## Arguments

- x:

  giotto object, a history from
  [`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md),
  or the path to a
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  folder or its `history.ndjson`

## Value

character vector of citations, one per method

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

objCitations(g)
```
