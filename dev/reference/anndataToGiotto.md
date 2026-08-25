# Convert anndata to Giotto

Converts a spatial anndata (e.g. scanpy) .h5ad file into a Giotto object

## Usage

``` r
anndataToGiotto(
  anndata_path = NULL,
  n_key_added = NULL,
  spatial_n_key_added = NULL,
  delaunay_spat_net = TRUE,
  spat_enrich_key_added = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  backend = NULL,
  h5_file = deprecated(),
  python_path = NULL,
  env_name = "giotto_env"
)
```

## Arguments

- anndata_path:

  path to the .h5ad file

- n_key_added:

  equivalent of "key_added" argument from scanpy.pp. neighbors(). If
  multiple spatial networks are in the anndata object, a list of
  key_added terms may be provided. If converting an anndata object from
  giottoToAnnData, the keys are saved in `.uns['NN_keys']` and all keys
  are used in conversion unless specified in the function call. Cannot
  be "spatial". This becomes the name of the nearest network in the
  gobject.

- spatial_n_key_added:

  equivalent of "key_added" argument from squidpy.gr.spatial_neighbors.
  If multiple spatial networks are in the anndata object, a list of
  key_added terms may be provided. If converting an anndata object from
  giottoToAnnData, the keys are saved in `.uns['SN_keys']` and all keys
  are used in conversion unless specified in the function call. Cannot
  be the same as n_key_added.

- delaunay_spat_net:

  binary parameter for spatial network. If TRUE, the spatial network is
  a delaunay network.

- spat_enrich_key_added:

  list of names of spatial enrichment annotations present in the anndata
  object. If converting an anndata object from giottoToAnnData and the
  original Giotto object had spatial enrichment annotations, the keys
  are saved in `.uns['SE_keys']` and all keys are used in conversion
  unless specified in the function call.

- spat_unit:

  desired spatial unit to use for conversion, default NULL

- feat_type:

  desired feature type to use for conversion, default NULL

- backend:

  path or `gsource` to use as a managed backend for on-disk artifacts
  (passed to
  [`createGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)).

- h5_file:

  deprecated. Use `backend` instead.

- python_path:

  path to python executable within a conda/miniconda environment

- env_name:

  name of environment containing python_path executable

## Value

Giotto object

## Details

Function in beta. Converts a .h5ad file into a Giotto object. The
returned Giotto Object will take default instructions with the exception
of the python path, which may be customized. See
[`changeGiottoInstructions`](https://giotto-suite.github.io/GiottoClass/dev/reference/changeGiottoInstructions.md)
to modify instructions after creation.
