# Convert Giotto to SpatialData

Converts a Giotto object to SpatialData object

## Usage

``` r
giottoToSpatialData(
  gobject = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  spot_radius = NULL,
  python_path = NULL,
  env_name = NULL,
  save_directory = NULL
)
```

## Arguments

- gobject:

  giotto object to be converted

- spat_unit:

  spatial unit which will be used in conversion

- feat_type:

  feature type which will be used in conversion

- spot_radius:

  radius of the spots

- python_path:

  path to python executable within a conda/miniconda environment

- env_name:

  name of environment containing python_path executable

- save_directory:

  directory in which the SpatialData object will be saved

## Value

SpatialData object saved on disk.

## Details

Function in beta. Converts and saves a Giotto object in SpatialData
format on disk.
