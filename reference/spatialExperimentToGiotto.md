# Utility function to convert a SpatialExperiment object to a Giotto object

Utility function to convert a SpatialExperiment object to a Giotto
object

## Usage

``` r
spatialExperimentToGiotto(
  spe,
  python_path,
  nn_network = NULL,
  sp_network = NULL,
  verbose = TRUE
)
```

## Arguments

- spe:

  Input SpatialExperiment object to convert to a Giotto object.

- python_path:

  Specify the path to python.

- nn_network:

  Specify the name of the nearest neighbour network(s) in the input
  SpatialExperiment object. Default `NULL` will use all existing
  networks.

- sp_network:

  Specify the name of the spatial network(s) in the input
  SpatialExperiment object. Default `NULL` will use all existing
  networks. This can be a vector of multiple network names.

- verbose:

  A boolean value specifying if progress messages should be displayed or
  not. Default `TRUE`.

## Value

Giotto object

## Examples

``` r
if (FALSE) { # \dontrun{
library(SpatialExperiment)
example(read10xVisium, echo = FALSE)
spatialExperimentToGiotto(spe)
} # }
```
