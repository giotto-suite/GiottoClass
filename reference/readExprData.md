# Read expression data

Read a nested list of expression data inputs in order to generate a list
of giotto-native exprObj

## Usage

``` r
readExprData(
  data_list,
  sparse = TRUE,
  cores = determine_cores(),
  default_feat_type = NULL,
  verbose = TRUE,
  provenance = NULL,
  expression_matrix_class = c("dgCMatrix", "DelayedArray")
)
```

## Arguments

- data_list:

  (nested) list of expression input data

- sparse:

  (boolean, default = TRUE) read matrix data in a sparse manner

- cores:

  number of cores to use

- default_feat_type:

  (optional) default feat_type to use

- verbose:

  be verbose

- provenance:

  (optional) provenance information

- expression_matrix_class:

  class of expression matrix to use (e.g. 'dgCMatrix', 'DelayedArray')

## Value

exprObj

## Details

mylistA = list('a' = matrix(seq(5)), 'b' = matrix(seq(5)))
depth(mylistA)

mylistB = list(A = list('a' = matrix(seq(5)), 'b' = matrix(seq(5))), B =
list('c' = matrix(seq(5)),'d' = matrix(seq(5)))) depth(mylistB)

mylistC = list('RNA' = list('RAW' = list('cell' = matrix(seq(5)),
'nucleus' = matrix(seq(6,10))), 'NORM' = list('cell' =
matrix(seq(11,15)), 'nucleus' = matrix(seq(20,25)))), 'PROT' =
list('RAW' = list('cell' = matrix(seq(16,20))))) depth(mylistC)

mymatD = matrix(data = seq(4))

## Examples

``` r
x <- matrix(seq_len(100), nrow = 10)
temporal_dir <- tempdir()
write.csv(x, paste0(temporal_dir, "/mymatrix.csv"))

readExprData(paste0(temporal_dir, "/mymatrix.csv"))
#> list depth of 1
#> 
#> List item [1]:
#>  spat_unit: cell
#>  feat_type: rna
#>  name: raw
#> Warning: [createExprObj] param 'expression_matrix_class' is deprecated
#> [[1]]
#> An object of class exprObj : "raw"
#> spat_unit : "cell"
#> feat_type : "rna"
#> provenance: cell 
#> 
#> contains:
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>                               
#> 1 1 11 21 31 41 51 61 71 81 91
#> 2 2 12 22 32 42 52 62 72 82 92
#> 3 3 13 23 33 43 53 63 73 83 93
#> 4 4 14 24 34 44 54 64 74 84 94
#> 
#>  ........suppressing 2 rows 
#>                                  
#> 7   7 17 27 37 47 57 67 77 87  97
#> 8   8 18 28 38 48 58 68 78 88  98
#> 9   9 19 29 39 49 59 69 79 89  99
#> 10 10 20 30 40 50 60 70 80 90 100
#> 
#>  First four colnames:
#>  V1 V2 V3 V4 
#> 
```
