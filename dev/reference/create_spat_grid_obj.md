# Create S4 spatialGridObj

Create S4 spatialGridObj

## Usage

``` r
create_spat_grid_obj(
  name = "test",
  method = NA_character_,
  parameters = NULL,
  gridDT = NULL,
  spat_unit = "cell",
  feat_type = "rna",
  provenance = NULL,
  misc = NULL
)
```

## Arguments

- name:

  name of spatialGridObj

- method:

  method used to generate spatial grid

- parameters:

  additional method-specific parameters used during spatial grid
  generation

- gridDT:

  data.table holding the spatial grid information

- spat_unit:

  spatial unit

- feat_type:

  feature type

- provenance:

  origin of aggregated information (if applicable)

- misc:

  misc

## Value

spatialGridObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatialGridObj")

create_spat_grid_obj(name = "test", gridDT = x)
```
