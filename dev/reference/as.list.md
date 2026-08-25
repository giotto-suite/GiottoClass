# Coerce to a list

Generic to coerce to a list if possible. Used with the `giotto` object,
it disassembles it into a list of subobjects.

## Usage

``` r
# S4 method for class 'giotto'
as.list(x, slots, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
```

## Arguments

- x:

  the object to coerce

- slots:

  character vector. Which data slots to include in list. See details

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type to use (e.g. "rna", "protein")

- name:

  name of the elements to select from the slot

- ...:

  additional arguments

## Value

list

## Details

- Giotto method - the slots argument currently accepts any or multiple
  of:
  `"spatial_info", "spatial_locs", "spatial_network", "feat_info", "expression", "cell_metadata", "feat_metadata", "spatial_enrichment", "nn_network", "dimension_reduction", "multiomics"`
