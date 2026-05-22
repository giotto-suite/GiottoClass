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
temporal_dir <- tempdir()
write.csv(df, paste0(temporal_dir, "/spatlocs.csv"), row.names = FALSE)

readSpatLocsData(paste0(temporal_dir, "/spatlocs.csv"))
#> list depth of 1
#> 
#> List item [1]:
#>  spat_unit: cell
#>  name: raw
#> [[1]]
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 3 3 
#> preview   :
#>    sdimx sdimy cell_ID
#>    <int> <int>  <char>
#> 1:     1     1  feat_1
#> 2:     2     2  feat_2
#> 3:     3     1  feat_3
#> 
#> ranges:
#>      sdimx sdimy
#> [1,]     1     1
#> [2,]     3     2
#> 
#> 
```
