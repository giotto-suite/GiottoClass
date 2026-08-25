# deprecated

Function to replace all instructions from giotto object. Does not call
`initialize` on the giotto object

## Usage

``` r
replaceGiottoInstructions(gobject, instructions = NULL, init_gobject = TRUE)
```

## Arguments

- gobject:

  giotto object

- instructions:

  new instructions (e.g. result from createGiottoInstructions)

- init_gobject:

  (boolean, default = TRUE) initialize gobject when returning

## Value

giotto object with replaces instructions

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

replaceGiottoInstructions(
    gobject = g,
    instructions = createGiottoInstructions()
)
```
