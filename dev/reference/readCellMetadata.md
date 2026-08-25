# Read cell metadata

read cell metadata from list

## Usage

``` r
readCellMetadata(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  nested list of cell metadata information

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

cell metadata

## Examples

``` r
df <- data.frame(
    cell_ID = c("cell_1", "cell_2", "cell_3"),
    annotation = c("cell_type_1", "cell_type_1", "cell_type_2")
)
f <- tempdir()
write.csv(df, paste0(f, "/metadata.csv"), row.names = FALSE)

readCellMetadata(paste0(f, "/metadata.csv"))
```
