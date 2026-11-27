# Show the citation of a method

Shows the citation of a method once per R session. Functions that record
their param with
[`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md)
do not need to call this; it is for functions that do not return a
giotto object.

## Usage

``` r
citeMethod(x, what = NULL)
```

## Arguments

- x:

  a param object with a
  [`methodCitation()`](https://giotto-suite.github.io/GiottoClass/dev/reference/methodCitation.md),
  or a `bibentry`

- what:

  character. Name of the method, used in the message

## Value

the citation text, invisibly

## Examples

``` r
cit <- utils::bibentry("Article",
    title = "A toy method", author = "Ann Author",
    journal = "Journal of Toys", year = 2024
)
citeMethod(cit, what = "toyMethod")
citeMethod(cit, what = "toyMethod") # already shown: silent
```
