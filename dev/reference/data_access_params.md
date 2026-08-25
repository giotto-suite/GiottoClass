# data_access_params

data_access_params

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- return_uniques:

  return unique nesting names (ignores if final object exists/is correct
  class)

- output:

  what format in which to get information (e.g. "data.table")

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

- copy_obj:

  whether to deep copy/duplicate when getting the object (default =
  TRUE)

- initialize:

  (default = FALSE) whether to initialize the gobject before returning

- ...:

  additional params to pass

## Value

data.table
