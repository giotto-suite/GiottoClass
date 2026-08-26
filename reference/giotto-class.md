# S4 giotto Class

Giotto's core object that encapsulates all the components of a
spatial-omic project and facilitates analyses.

## Value

giotto object

## Details

\[**initialize**\] The `giotto` class has a robust `initialize()` method
that is automatically called upon setting data into the object, updates
of the `giottoInstructions`, and loading of saved objects. It performs
the following steps:

1.  Update the object and subobjects for class definition changes if
    needed

2.  Ensure a set of `giottoInstructions` are available, otherwise
    generate defaults

3.  Ensure a giotto python environment is accessible when the options
    giotto.has_conda and giotto.use_conda are TRUE

4.  Check the active spat_unit and feat_type

5.  Ensure spatial/cell ID consistency and initialize the cell_ID and
    feat_ID slots for the active spat_unit and feat_type, as well as
    cell and feature metadata if they do not exist. Values for IDs and
    metadata are pulled from any existing data in spatial_info/feat_info
    or expression slots, with a preference for the latter.

6.  Perform slot-specific and hierarchical checks that ensure dependent
    pieces of information are only added AFTER the data that they depend
    on and that existing information is consistent across slots.

7.  Object validity checking

## Slots

- `expression`:

  expression information

- `expression_feat`:

  The different features or modalities such as rna, protein,
  metabolites, ... that are provided in the expression slot.

- `spatial_locs`:

  spatial location coordinates for cells/spots/grids

- `spatial_info`:

  information about spatial units (Giotto spatVector)

- `cell_metadata`:

  metadata for cells

- `feat_metadata`:

  metadata for available features

- `feat_info`:

  information about features (Giotto spatVector)

- `cell_ID`:

  unique cell IDs

- `feat_ID`:

  unique feature IDs for all features or modalities

- `spatial_network`:

  spatial network in data.table/data.frame format

- `spatial_grid`:

  spatial grid in data.table/data.frame format

- `spatial_enrichment`:

  slot to save spatial enrichment-like results

- `dimension_reduction`:

  slot to save dimension reduction coordinates

- `nn_network`:

  nearest neighbor network in igraph format

- `images`:

  slot to store giotto image objects

- `parameters`:

  slot to save parameters that have been used

- `instructions`:

  slot for global function instructions

- `offset_file`:

  offset file used to stitch together image fields

- `versions`:

  giotto object metadata and versioning info

- `join_info`:

  information about joined Giotto objects

- `multiomics`:

  multiomics integration results

- `h5_file`:

  path to h5 file

- `misc`:

  miscellaneous or unstructured data

## Examples

``` r
giotto()
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
#> An object of class giotto 
#> [SUBCELLULAR INFO]
#> [AGGREGATE INFO]
#> 
#> 
#> Use objHistory() to see steps and params used
```
