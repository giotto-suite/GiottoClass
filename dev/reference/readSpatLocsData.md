# Read spatial location data

read spatial locations/coordinates from nested list and generate list of
Giotto spatLocsObj

## Usage

``` r
readSpatLocsData(
  data_list,
  default_spat_unit = NULL,
  provenance = NULL,
  cores = determine_cores(),
  verbose = TRUE
)
```

## Arguments

- data_list:

  (nested) list of spatial locations input data

- default_spat_unit:

  (optional) default spat_unit to use

- provenance:

  (optional) provenance information

- cores:

  how many cores to use

- verbose:

  be verbose

## Value

list of spatLocsObj

## Examples

``` r
df <- data.frame(
    feat_ID = c("feat_1", "feat_2", "feat_3"),
    sdimx = c(1, 2, 3), sdimy = c(1, 2, 1)
)
f <- tempdir()
write.csv(df, paste0(f, "/spatlocs.csv"), row.names = FALSE)

readSpatLocsData(paste0(f, "/spatlocs.csv"))
```
