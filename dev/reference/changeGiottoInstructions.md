# deprecated

Function to change one or more instructions from giotto object. If more
than one item is supplied to `params` and `new_values`, use a vector of
values. Does not call `initialize` on the giotto object

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

  new value(s) for parameter(s)

- return_gobject:

  (boolean, default = TRUE) return giotto object

- init_gobject:

  (boolean, default = TRUE) initialize gobject if returning

## Value

giotto object with one or more changed instructions

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

changeGiottoInstructions(
    gobject = g, params = "save_plot",
    new_values = TRUE
)
```
