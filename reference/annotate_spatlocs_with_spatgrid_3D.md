# annotate_spatlocs_with_spatgrid_3D

annotate spatial locations with 3D spatial grid information

## Usage

``` r
annotate_spatlocs_with_spatgrid_3D(spatloc, spatgrid)
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
g <- GiottoData::loadGiottoMini("starmap")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
g_spatloc <- getSpatialLocations(g, output = "data.table")
g_spatgrid <- getSpatialGrid(g)
#> The grid name was not specified, default to the first: "spatial_grid"

annotate_spatlocs_with_spatgrid_3D(
    spatloc = g_spatloc,
    spatgrid = g_spatgrid
)
#>       sdimx sdimy sdimz    cell_ID gr_x_loc gr_y_loc gr_z_loc gr_loc
#>       <int> <int> <int>     <char>   <char>   <char>   <char> <char>
#>    1:   270   212    19  cell_2944   gr_x_2   gr_y_2   gr_z_2  gr_83
#>    2:  1389  1215    54 cell_17523   gr_x_7   gr_y_7   gr_z_4 gr_277
#>    3:  1051   360    89 cell_30080   gr_x_6   gr_y_2   gr_z_5 gr_303
#>    4:   592   777    10   cell_941   gr_x_3   gr_y_5   gr_z_1  gr_39
#>    5:  1058   223    84 cell_28514   gr_x_6   gr_y_2   gr_z_5 gr_303
#>   ---                                                               
#> 3901:   348  1383    78 cell_26011   gr_x_2   gr_y_8   gr_z_5 gr_353
#> 3902:   674  1080    89 cell_31539   gr_x_4   gr_y_6   gr_z_5 gr_337
#> 3903:   919   532    22  cell_4185   gr_x_5   gr_y_3   gr_z_2  gr_95
#> 3904:   947   885    23  cell_3851   gr_x_5   gr_y_5   gr_z_2 gr_113
#> 3905:   144   439    60 cell_21337   gr_x_1   gr_y_3   gr_z_4 gr_235
```
