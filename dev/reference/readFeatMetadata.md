# Read feature metadata

read feature metadata from listt

## Usage

``` r
readFeatMetadata(
  data_list,
  default_spat_unit = NULL,
  default_feat_type = NULL,
  provenance = NULL,
  verbose = TRUE
)
```

## Arguments

- data_list:

  nested list of feature metadata information

- default_spat_unit:

  (optional) default spat_unit to use

- default_feat_type:

  (optional) default feat_type to use

- provenance:

  (optional) provenance information

- verbose:

  be verbose

## Value

featMetadata

## Examples

``` r
df <- data.frame(
    feat_ID = c("feat_1", "feat_2", "feat_3"),
    geneID = c("gene_1", "gene_1", "gene_2")
)
f <- tempdir()
write.csv(df, paste0(f, "/metadata.csv"), row.names = FALSE)

readFeatMetadata(paste0(f, "/metadata.csv"))
```
