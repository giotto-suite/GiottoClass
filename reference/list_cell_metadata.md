# list_cell_metadata

lists the available cell metadata.

## Usage

``` r
list_cell_metadata(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  return_uniques = FALSE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

## Value

names and locations of available cell metadata as data.table

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

list_cell_metadata(g)
#>    spat_unit feat_type
#>       <char>    <char>
#> 1:      cell       rna
```
