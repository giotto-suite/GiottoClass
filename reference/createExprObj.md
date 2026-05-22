# Create S4 exprObj

Create an `exprObj` (thin metadata wrapper/adapter) for Giotto
expression value information. `matrix` and tabular formats will be
coerced to Matrix sparse format `dgCMatrix`. For tabular inputs, the
first column is used as row names.

### Alternative and disk-backed formats

Pre-constructed backed matrices (`HDF5Array`, `dbMatrix`,
`tiledb_array`, `IterableMatrix`) may also be passed directly to
`expression_data` and will be used as-is.

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
#> An object of class exprObj : "test"
#> spat_unit : "cell"
#> feat_type : "rna"
#> 
#> contains:
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>                          
#> feat1 . 1 2 3 3 . . . 1 1
#> feat2 2 . 2 1 . 1 1 1 3 1
#> feat3 1 1 . 1 2 2 1 1 2 .
#> feat4 1 2 . 1 . 1 1 2 1 .
#> 
#>  ........suppressing 2 rows 
#>                           
#> feat7  2 1 . 2 . 1 1 1 . 3
#> feat8  2 1 . . . . 1 1 1 .
#> feat9  1 2 2 . . 1 1 1 1 1
#> feat10 3 . 1 . . 4 1 1 2 2
#> 
#>  First four colnames:
#>  cell1 cell2 cell3 cell4 

# example 0-dominated sparse matrix
x_expr <- readRDS(system.file("extdata/toy_matrix.RDS",
    package = "GiottoClass"
))
createExprObj(expression_data = x_expr)
#> An object of class exprObj : "test"
#> spat_unit : "cell"
#> feat_type : "rna"
#> 
#> contains:
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>                      
#> a . . . . . . . . . .
#> b . . . . 1 . . . . .
#> c . . . . . . 1 . . .
#> d . . . . . . . . . 1
#> 
#>  ........suppressing 2 rows 
#>                      
#> g 1 . . . . . . . . .
#> h 1 . . . . . . . . 1
#> i . . . . . . . . . .
#> j . . . 1 . 1 . . . .
#> 
#>  First four colnames:
#>  A B C D 
```
