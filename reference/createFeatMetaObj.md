# Create S4 featMetaObj

Create an S4 featMetaObj

## Usage

``` r
createFeatMetaObj(
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

featMetaObj

## Examples

``` r
x <- data.frame(
    feat_ID = c("Mlc1", "Gprc5b", "Gfap"),
    nr_cells = c(80, 138, 151)
)

createFeatMetaObj(metadata = x)
#> An object of class featMetaObj 
#> spat_unit : "cell"
#> feat_type : "rna"
#> 
#>    feat_ID nr_cells
#>     <char>    <num>
#> 1:    Mlc1       80
#> 2:  Gprc5b      138
#> 3:    Gfap      151
```
