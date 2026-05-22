# Convert Giotto to anndata

Converts a Giotto object to a spatial anndata (e.g. scanpy) .h5ad file

## Usage

``` r
giottoToAnnData(
  gobject = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  python_path = NULL,
  env_name = "giotto_env",
  save_directory = NULL
)
```

## Arguments

- gobject:

  giotto object to be converted

- spat_unit:

  spatial unit which will be used in conversion.

- feat_type:

  feature type which will be used in conversion.

- python_path:

  path to python executable within a conda/miniconda environment

- env_name:

  name of environment containing python_path executable

- save_directory:

  directory in which the file will be saved.

## Value

vector containing .h5ad file path(s)

## Details

Function in beta. Converts a Giotto object into .h5ad file(s).

If there are multiple spatial units and/or feature types, but only one
spatial unit and/or feature type is specified, then only the specified
spatial unit and/or feature type will be used. If NULL, by default, all
spatial units will be used in conversion.

If multiple spatial units or feature types are specified, multiple
AnnData object will be created and returned.

This function will create .txt files which will record any `key_added`
parameters for networks. They are named after the corresponding spatial
unit and feature type pair.

The save_directory will be created if it does not already exist. The
default save_directory is the working directory.
