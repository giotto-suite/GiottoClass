# Read a giotto instruction

Deprecated. Use
[`instructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
instead.

## Usage

``` r
readGiottoInstructions(giotto_instructions, param = NULL, default)
```

## Arguments

- giotto_instructions:

  giotto object or a `giottoInstructions` list

- param:

  parameter to retrieve

- default:

  value to return when `param` is absent. When missing, an absent
  `param` is an error.

## Value

the value of the requested instruction param
