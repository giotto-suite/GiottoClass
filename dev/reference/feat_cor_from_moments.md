# Feature correlation from moment sums

The Pearson correlation between features from \\X X^T\\, the row sums of
\\X\\ and the number of columns – the sums a streaming backend
accumulates for
[`featCorParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/featCorParam.md).
Features without variance get `NA`.

## Usage

``` r
feat_cor_from_moments(G, s, n, names = NULL)
```

## Arguments

- G:

  features x features matrix \\X X^T\\

- s:

  per-feature sums

- n:

  number of cells

- names:

  feature names

## Value

a features x features correlation matrix

## Examples

``` r
x <- matrix(rnorm(30), 3)
feat_cor_from_moments(tcrossprod(x), rowSums(x), ncol(x))
```
