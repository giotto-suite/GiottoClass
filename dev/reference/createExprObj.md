# Create S4 exprObj

Create an `exprObj` (thin metadata wrapper/adapter) for Giotto
expression value information. `matrix` and tabular formats will be
coerced to Matrix sparse format `dgCMatrix`. For tabular inputs, the
first column is used as row names.

### Alternative and disk-backed formats

Pre-constructed backed matrices (`HDF5Array`, `dbMatrix`,
`tiledb_array`, `IterableMatrix`) may also be passed directly to
`expression_data` and will be used as-is. If using GiottoDisk,
expression values will be written to the configured backend format when
set into the `giotto` object, so there is no need to construct a backed
matrix upfront — pass a regular matrix and let the gsource vault handle
persistence.

## Usage

``` r
createExprObj(
  expression_data,
  name = "test",
  spat_unit = "cell",
  feat_type = "rna",
  provenance = NULL,
  misc = NULL,
  expression_matrix_class = deprecated()
)
```

## Arguments

- expression_data:

  expression data. Accepts matrix-like data and coercible `data.frame`
  formats (first column used as row names). A filepath to a flat file
  readable by
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  can also be passed.

- name:

  name of exprObj

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- provenance:

  origin data of expression information (if applicable)

- misc:

  misc about the object to attach

- expression_matrix_class:

  deprecated. Previously wrapped the matrix in a `DelayedArray` when
  selected. Moving forward, pass a pre-constructed backed matrix to
  `expression_data` directly if needed.

## Value

`exprObj` wrapping the expression matrix with `spat_unit`, `feat_type`,
and provenance metadata attached.

## Examples

``` r
m <- matrix(rpois(100, 1), nrow = 10,
    dimnames = list(paste0("feat", 1:10), paste0("cell", 1:10))
)
createExprObj(m)

# example 0-dominated sparse matrix
x_expr <- readRDS(system.file("extdata/toy_matrix.RDS",
    package = "GiottoClass"
))
createExprObj(expression_data = x_expr)
```
