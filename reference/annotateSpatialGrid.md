# annotateSpatialGrid

annotate spatial grid with cell ID and cell metadata (optional)

## Usage

``` r
annotateSpatialGrid(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = "raw",
  spatial_grid_name = "spatial_grid",
  cluster_columns = NULL
)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations

- spatial_grid_name:

  name of spatial grid, see
  [`showGiottoSpatGrids`](https://giotto-suite.github.io/GiottoClass/reference/showGiottoSpatGrids.md)

- cluster_columns:

  names of cell metadata, see
  [`pDataDT`](https://giotto-suite.github.io/GiottoClass/reference/pDataDT.md)

## Value

annotated spatial grid data.table

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)

annotateSpatialGrid(g)
#>      sdimx sdimy            cell_ID gr_x_loc gr_y_loc    gr_loc
#>      <int> <int>             <char>   <char>   <char>    <char>
#>   1:  5477 -4125 AAAGGGATGTAGCAAG-1 gr_x_482 gr_y_264 gr_178007
#>   2:  5959 -2808 AAATGGCATGTCTTGT-1 gr_x_579 gr_y_528 gr_356304
#>   3:  4720 -5202 AAATGGTCAATGTGCC-1 gr_x_331  gr_y_49  gr_32731
#>   4:  5202 -5322 AAATTAACGGGTAGCT-1 gr_x_427  gr_y_25  gr_16627
#>   5:  4101 -4604 AACAACTGGTAGTTGC-1 gr_x_207 gr_y_169 gr_113607
#>  ---                                                           
#> 620:  4996 -5442 TTGTAATCCGTACTCG-1 gr_x_386   gr_y_1    gr_386
#> 621:  6303 -2688 TTGTATCACACAGAAT-1 gr_x_648 gr_y_552 gr_372573
#> 622:  5202 -3885 TTGTCGTTCAGTTACC-1 gr_x_427 gr_y_312 gr_210352
#> 623:  5340 -3406 TTGTGGCCCTGACAGT-1 gr_x_455 gr_y_408 gr_275180
#> 624:  5615 -4125 TTGTTCAGTGTGCTAC-1 gr_x_510 gr_y_264 gr_178035
```
