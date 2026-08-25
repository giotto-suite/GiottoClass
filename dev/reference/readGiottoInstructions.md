# deprecated

Retrieves the instruction associated with the provided parameter

## Usage

``` r
readGiottoInstructions(giotto_instructions, param = NULL, default)
```

## Arguments

- giotto_instructions:

  giotto object or result from createGiottoInstructions()

- param:

  parameter to retrieve

- default:

  default object to return if parameter to retrieve does not exist

## Value

specific parameter

## Examples

``` r
readGiottoInstructions(
    giotto_instructions = createGiottoInstructions(),
    param = "show_plot"
)
```
