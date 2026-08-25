# Read expression data

Read a nested list of expression data inputs in order to generate a list
of giotto-native exprObj that are addressed to specific spat_unit and
feat_type based on list naming and defaults.

## Usage

``` r
readExprData(
  data_list,
  sparse = TRUE,
  cores = determine_cores(),
  default_feat_type = NULL,
  verbose = TRUE,
  provenance = NULL,
  expression_matrix_class = deprecated()
)
```

## Arguments

- data_list:

  nested `list` of expression input data

- sparse:

  (`logical`, default = TRUE) read matrix data in a sparse manner

- cores:

  number of cores to use

- default_feat_type:

  (optional) default feat_type to use

- verbose:

  be verbose

- provenance:

  (optional) provenance information

- expression_matrix_class:

  deprecated. See
  [`?createExprObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/createExprObj.md)
  for details

## Value

exprObj

## Examples

``` r
mylistA = list('a' = matrix(seq(5)), 'b' = matrix(seq(5)))
GiottoUtils::depth(mylistA)

mylistB = list(
    A = list('a' = matrix(seq(5)), 'b' = matrix(seq(5))),
    B = list('c' = matrix(seq(5)),'d' = matrix(seq(5)))
)
GiottoUtils::depth(mylistB)

mylistC = list(
    'RNA' = list(
        'RAW' = list('cell' = matrix(seq(5)),
        'nucleus' = matrix(seq(6,10))),
        'NORM' = list('cell' = matrix(seq(11,15)),
        'nucleus' = matrix(seq(20,25)))
    ),
    'PROT' = list(
        'RAW' = list('cell' = matrix(seq(16,20)))
    )
)
GiottoUtils::depth(mylistC)

readExprData(mylistA)
readExprData(mylistB)
readExprData(mylistC)
```
