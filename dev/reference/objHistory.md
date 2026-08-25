# Giotto object history

Print and return giotto object history

## Usage

``` r
objHistory(object, summarized = FALSE)
```

## Arguments

- object:

  giotto object

- summarized:

  logical. whether print should be summarized

## Value

list

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

objHistory(g)
objHistory(g, summarized = TRUE)
```
