# Convert SpatialData to Giotto

Converts a saved SpatialData object into a Giotto object

## Usage

``` r
spatialdataToGiotto(
  spatialdata_path = NULL,
  n_key_added = NULL,
  spatial_n_key_added = NULL,
  delaunay_spat_net = TRUE,
  spat_enrich_key_added = NULL,
  spat_unit = NULL,
  feat_type = NULL,
  python_path = NULL,
  env_name = NULL
)
```

## Arguments

- spatialdata_path:

  path to SpatialData object

- n_key_added:

  equivalent of "key_added" argument from scanpy.pp.neighbors(). If
  multiple spatial networks are in the anndata object, a list of
  key_added terms may be provided. If converting an anndata object from
  giottoToAnnData, the keys are saved in `.uns['NN_keys']` and all keys
  are used in conversion unless specified in the function call. Cannot
  be "spatial". This becomes the name of the nearest network in the
  gobject.

- spatial_n_key_added:

  equivalent of "key_added" argument from squidpy.gr.spatial_neighbors.
  If multiple spatial networks are in the anndata object, a list of
  key_added terms may be provided. If converting a SpatialData object
  from giottoToSpatialData, the keys are saved in `.uns['SN_keys']` and
  all keys are used in conversion unless specified in the function call.
  Cannot be the same as n_key_added.

- delaunay_spat_net:

  binary parameter for spatial network. If TRUE, the spatial network is
  a delaunay network.

- spat_enrich_key_added:

  list of names of spatial enrichment annotations added to the
  SpatialData object. If converting an anndata object from
  giottoToAnnData and the original Giotto object had spatial enrichment
  annotations, the keys are saved in `.uns['SE_keys']` and all keys are
  used in conversion unless specified in the function call.

- spat_unit:

  desired spatial unit for conversion, default NULL

- feat_type:

  desired feature type for conversion, default NULL

- python_path:

  path to python executable within a conda/miniconda environment

- env_name:

  name of environment containing python_path executable

## Value

Giotto object

## Details

Function in beta. Converts a structured SpatialData file into a Giotto
object. The returned Giotto Object will take default insructions with
the exception of the python path, which may be customized. See
[`changeGiottoInstructions`](https://giotto-suite.github.io/GiottoClass/reference/changeGiottoInstructions.md)
to modify instructions after creation.
