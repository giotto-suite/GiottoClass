# list_expression

lists the available matrices

## Usage

``` r
list_expression(gobject, spat_unit = NULL, feat_type = NULL)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

## Value

names and locations of available matrices as data.table. col order
matters.

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

list_expression(g)
#>    spat_unit feat_type       name
#>       <char>    <char>     <char>
#> 1:      cell       rna        raw
#> 2:      cell       rna normalized
#> 3:      cell       rna     scaled
```
