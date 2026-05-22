# Create S4 cellMetaObj

Create an S4 cellMetaObj

## Usage

``` r
createCellMetaObj(
  metadata,
  spat_unit = "cell",
  feat_type = "rna",
  provenance = NULL,
  col_desc = NULL,
  verbose = TRUE
)
```

## Arguments

- metadata:

  metadata info

- spat_unit:

  spatial unit of aggregated expression (e.g. 'cell')

- feat_type:

  feature type of aggregated expression (e.g. 'rna', 'protein')

- provenance:

  origin data of aggregated expression information (if applicable)

- col_desc:

  (optional) character vector describing columns of the metadata

- verbose:

  be verbose

## Value

cellMetaObj

## Examples

``` r
df <- data.frame(cell_ID = c("cell_1", "cell_2", "cell_3"))

createCellMetaObj(metadata = df)
#> An object of class cellMetaObj 
#> spat_unit : "cell"
#> feat_type : "rna"
#> dimensions: 3 1 
#> 
#>    cell_ID
#>     <char>
#> 1:  cell_1
#> 2:  cell_2
#> 3:  cell_3
```
