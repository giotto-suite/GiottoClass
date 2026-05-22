# annotate_spatlocs_with_spatgrid_2D

annotate spatial locations with 2D spatial grid information

## Usage

``` r
annotate_spatlocs_with_spatgrid_2D(spatloc, spatgrid)
```

## Arguments

- spatloc:

  spatial_locs slot from giotto object

- spatgrid:

  selected spatial_grid slot from giotto object

## Value

annotated spatial location data.table

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
g_spatloc <- getSpatialLocations(g, output = "data.table")
g_spatgrid <- getSpatialGrid(g)
#> The grid name was not specified, default to the first: "spatial_grid"

annotate_spatlocs_with_spatgrid_2D(
    spatloc = g_spatloc,
    spatgrid = g_spatgrid
)
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
