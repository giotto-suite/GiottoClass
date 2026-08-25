# Evaluate raw inputs to Giotto formatting

Experimental. Evaluate raw inputs into formats that are directly
compatible with Giotto's functionality. Note that this function only
formats the data. The output from this function still needs to be put
into a Giotto subobject.  
This is a wrapper function for the individual GiottoClass evaluation
functions.

## Usage

``` r
evaluate_input(type, x, ...)
```

## Arguments

- type:

  character. Type of giotto data to evaluate to.

- x:

  data to evaluate

- ...:

  additional params to pass

## Value

character or the same class of x

## Examples

``` r
x <- GiottoData::loadSubObjectMini("exprObj", 1)

evaluate_input(type = "expression", x)
```
