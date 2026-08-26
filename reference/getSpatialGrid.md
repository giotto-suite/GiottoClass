# Get spatial grid

Function to get spatial grid

## Usage

``` r
getSpatialGrid(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  name = NULL,
  return_grid_Obj = FALSE,
  set_defaults = TRUE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type (e.g. "rna", "dna", "protein")

- name:

  name of spatial grid

- return_grid_Obj:

  return grid object (default = FALSE)

- set_defaults:

  set default spat_unit and feat_type. Change to FALSE only when
  expression and spat_info are not expected to exist.

## Value

spatialGridObj

## See also

Other spatial grid data accessor functions:
[`setSpatialGrid()`](https://giotto-suite.github.io/GiottoClass/reference/setSpatialGrid.md)

Other functions to get data from giotto object:
[`getCellMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getCellMetadata.md),
[`getDimReduction()`](https://giotto-suite.github.io/GiottoClass/reference/getDimReduction.md),
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/reference/getExpression.md),
[`getFeatureInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureInfo.md),
[`getFeatureMetadata()`](https://giotto-suite.github.io/GiottoClass/reference/getFeatureMetadata.md),
[`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/reference/getGiottoImage.md),
[`getMultiomics()`](https://giotto-suite.github.io/GiottoClass/reference/getMultiomics.md),
[`getNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getNearestNetwork.md),
[`getPolygonInfo()`](https://giotto-suite.github.io/GiottoClass/reference/getPolygonInfo.md),
[`getSpatialEnrichment()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialEnrichment.md),
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialLocations.md),
[`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/reference/getSpatialNetwork.md),
[`get_multiomics()`](https://giotto-suite.github.io/GiottoClass/reference/get_multiomics.md)

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)

getSpatialGrid(g)
#> The grid name was not specified, default to the first: "spatial_grid"
#>         x_start y_start  x_end y_end   gr_name gr_x_name gr_y_name
#>           <num>   <num>  <num> <num>    <char>    <char>    <char>
#>      1:  3067.5   -5445 3072.5 -5440      gr_1    gr_x_1    gr_y_1
#>      2:  3072.5   -5445 3077.5 -5440      gr_2    gr_x_2    gr_y_1
#>      3:  3077.5   -5445 3082.5 -5440      gr_3    gr_x_3    gr_y_1
#>      4:  3082.5   -5445 3087.5 -5440      gr_4    gr_x_4    gr_y_1
#>      5:  3087.5   -5445 3092.5 -5440      gr_5    gr_x_5    gr_y_1
#>     ---                                                           
#> 388796:  6417.5   -2570 6422.5 -2565 gr_388796  gr_x_671  gr_y_576
#> 388797:  6422.5   -2570 6427.5 -2565 gr_388797  gr_x_672  gr_y_576
#> 388798:  6427.5   -2570 6432.5 -2565 gr_388798  gr_x_673  gr_y_576
#> 388799:  6432.5   -2570 6437.5 -2565 gr_388799  gr_x_674  gr_y_576
#> 388800:  6437.5   -2570 6442.5 -2565 gr_388800  gr_x_675  gr_y_576
```
