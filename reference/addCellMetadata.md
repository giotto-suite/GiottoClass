# Add cell metadata

Adds cell metadata to the giotto object

## Usage

``` r
addCellMetadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  new_metadata,
  vector_name = NULL,
  by_column = FALSE,
  column_cell_ID = NULL
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- new_metadata:

  new cell metadata to use (data.table, data.frame, vector, factor, ...)

- vector_name:

  (optional) custom name for new metadata column if single vector or
  factor is provided

- by_column:

  merge metadata based on *cell_ID* column in
  [`pDataDT`](https://giotto-suite.github.io/GiottoClass/reference/pDataDT.md)
  (default = FALSE)

- column_cell_ID:

  column name of new metadata to use if `by_column = TRUE`

## Value

giotto object

## Details

You can add additional cell metadata in several manners:

- 1\. Provide a data.frame-like object, vector, or factor with cell
  annotations in the same order as the *cell_ID* column in
  pDataDT(gobject). This is a bit risky and not the most recommended.

- 2\. Provide a data.frame-like object with cell annotations and specify
  which column contains the cell IDs, these cell IDs need to match with
  the *cell_ID* column in pDataDT(gobject)

- 3\. Provide a vector or factor that is named with the cell IDs they
  correspond to. These names will be matched against the *cell_ID*
  column in pDataDT(gobject).

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

m <- pDataDT(g)
m <- m[, c("cell_ID", "leiden_clus")]
m$cell_type <- paste0("cell_type_", m$leiden_clus)
m <- m[, c("cell_ID", "cell_type")]

g <- addCellMetadata(
    g,
    new_metadata = m,
    by_column = TRUE,
    column_cell_ID = "cell_ID"
)

pDataDT(g)
#>                 cell_ID in_tissue nr_feats perc_feats total_expr leiden_clus
#>                  <char>     <int>    <int>      <num>      <num>       <num>
#>   1: AACTCGATGGCGCAGT-1         1      265   41.79811  1057.9308           2
#>   2: GGCTGGCTAGCTTAAA-1         1      279   44.00631  1064.7493           5
#>   3: GACGCCTGTTGCAGGG-1         1      219   34.54259   964.9294           2
#>   4: GAGGGCATCGCGTATC-1         1      294   46.37224  1142.7664           2
#>   5: TCAACACATTGGGTAA-1         1      261   41.16719  1063.3517           2
#>  ---                                                                        
#> 620: GGTAGTGCTCGCACCA-1         1      179   28.23344   768.4749           5
#> 621: AAGCTCGTGCCAAGTC-1         1      195   30.75710   756.0675           5
#> 622: TATTCAATTCTAATCC-1         1      247   38.95899   921.8264           5
#> 623: TTCAAAGTCTCTAGCC-1         1      384   60.56782   916.5929           6
#> 624: TTGAATATGGACTTTC-1         1      380   59.93691   912.3051           6
#>      custom_leiden   cell_type
#>              <num>      <char>
#>   1:             4 cell_type_2
#>   2:             3 cell_type_5
#>   3:             3 cell_type_2
#>   4:             3 cell_type_2
#>   5:             3 cell_type_2
#>  ---                          
#> 620:             4 cell_type_5
#> 621:             4 cell_type_5
#> 622:             4 cell_type_5
#> 623:             7 cell_type_6
#> 624:             4 cell_type_6
```
