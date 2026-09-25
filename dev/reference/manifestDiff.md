# Diff two giotto manifests

Compare two manifests from
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)
and report what changed, both as data and as a single human-readable
sentence. Pure: it reads manifests only, never a `giotto` object.

This is what an execution tool attaches to each result so a model, a
critic or a scorer knows what an operation actually did, rather than
inferring it from printed console output.

## Usage

``` r
manifestDiff(before, after)
```

## Arguments

- before:

  `gmanifest` or `NULL` (treated as "object did not exist")

- after:

  `gmanifest`

## Value

list with `changed` (logical), `summary` (character scalar) and `detail`
(added / removed / modified / object)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

before <- objManifest(g)
g <- subsetGiotto(g, cell_ids = head(spatIDs(g), 100))
manifestDiff(before, objManifest(g))$summary
```
