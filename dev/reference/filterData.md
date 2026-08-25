# Data Filter

Generic for filtering an object containing measured values, producing a
selection (typically a list of IDs to keep) rather than transformed data
or summary statistics. Specific methods should be defined for this
generic to perform filtering specific to a data class type. No methods
are exported from GiottoClass. The methods, which may differ depending
on the input data, are attached from other packages which focus on
filtering and/or alternative data representations with specific ways to
implement those filters.

## Usage

``` r
filterData(x, param, ...)
```

## Arguments

- x:

  a data object

- param:

  a
  [filterParam](https://giotto-suite.github.io/GiottoClass/dev/reference/filterParam-class.md)
  inheriting object

- ...:

  additional arguments, for use in specific methods

## Value

A selection (typically a list of character ID vectors)
