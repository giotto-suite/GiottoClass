# Citation of a method

The publication to cite for a method that runs through a param object. A
package that wraps or reimplements a published method defines a
`methodCitation()` method for the method's param class, returning a
[`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html).

When the param is attached to the object history
(`update_giotto_params(attachments = list(param = param))`), the
citation is recorded with that step and shown once per session.
[`objCitations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objCitations.md)
lists the citations of every recorded step. Set
`options(giotto.cite = FALSE)` to turn the messages off.

## Usage

``` r
# S4 method for class 'ANY'
methodCitation(x, ...)
```

## Arguments

- x:

  a param object

- ...:

  additional params (none implemented)

## Value

a `bibentry`, or `NULL` when the method has no citation

## Examples

``` r
setClass("toyParam", contains = "analyzeParam")
setMethod("methodCitation", "toyParam", function(x, ...) {
    utils::bibentry("Article",
        title = "A toy method", author = "Ann Author",
        journal = "Journal of Toys", year = 2024
    )
})
methodCitation(new("toyParam"))
```
