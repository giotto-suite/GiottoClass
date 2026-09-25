# Change giotto instructions

Deprecated. Use `instructions(gobject, param) <- value` instead.

## Usage

``` r
changeGiottoInstructions(
  gobject,
  params = NULL,
  new_values = NULL,
  return_gobject = TRUE,
  init_gobject = TRUE
)
```

## Arguments

- gobject:

  giotto object

- params:

  parameter(s) to change

- new_values:

  new value(s) for `params`

- return_gobject:

  logical. Return the giotto object (default `TRUE`) rather than the
  instructions list alone.

- init_gobject:

  logical. Re-initialize the object when returning it (default `TRUE`)

## Value

giotto object with changed instructions, or the instructions list when
`return_gobject = FALSE`
