# Replace giotto instructions

Deprecated. Use `instructions(gobject) <- value` instead.

## Usage

``` r
replaceGiottoInstructions(gobject, instructions = NULL, init_gobject = TRUE)
```

## Arguments

- gobject:

  giotto object

- instructions:

  named list of all instructions, as produced by
  [`createGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)

- init_gobject:

  logical. Re-initialize the object before returning it (default `TRUE`)

## Value

giotto object with replaced instructions
