# Read expression matrix

Attempts to read a data.table compatible flat file (.csv/.tsv) as
`dgCMatrix`

## Usage

``` r
readExprMatrix(
  path,
  cores = determine_cores(),
  transpose = FALSE,
  feat_type = "rna",
  expression_matrix_class = deprecated()
)
```

## Arguments

- path:

  `character` path to the expression matrix

- cores:

  `integerlike` number of cores to use with data.table read

- transpose:

  `logical` transpose matrix

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- expression_matrix_class:

  deprecated. See
  [`?createExprObj`](https://giotto-suite.github.io/GiottoClass/dev/reference/createExprObj.md)
  for details

## Value

sparse matrix

## Details

The expression matrix needs to have both unique column names and row
names

## Examples

``` r
x <- matrix(seq_len(100), nrow = 10)
f <- tempdir()
write.csv(x, paste0(f, "/mymatrix.csv"))

readExprMatrix(paste0(f, "/mymatrix.csv"))
```
