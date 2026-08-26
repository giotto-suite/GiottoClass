# combineCellData

Produce a table of information about the cells, including the geometry
and centroids information. This function will be simplified in the
future with
[`spatValues()`](https://giotto-suite.github.io/GiottoClass/reference/spatValues.md).

## Usage

``` r
combineCellData(
  gobject,
  feat_type = "rna",
  include_spat_locs = TRUE,
  spat_loc_name = "raw",
  include_poly_info = TRUE,
  poly_info = "cell",
  include_spat_enr = TRUE,
  spat_enr_names = NULL,
  ext = NULL,
  xlim = NULL,
  ylim = NULL,
  remove_background_polygon = TRUE
)
```

## Arguments

- gobject:

  giotto object

- feat_type:

  feature type

- include_spat_locs:

  include information about spatial locations

- spat_loc_name:

  spatial location name

- include_poly_info:

  include information about polygon

- poly_info:

  polygon information name

- include_spat_enr:

  include information about spatial enrichment

- spat_enr_names:

  names of spatial enrichment results to include

- ext:

  numeric or SpatExtent (optional). A cropping extent to apply to to the
  geometries.

- xlim, ylim:

  numeric length of 2 (optional). x or y bounds to apply.

- remove_background_polygon:

  logical (default = `TRUE`).
  [`crop()`](https://giotto-suite.github.io/GiottoClass/reference/crop.md)
  may sometimes produce extent-filling polygons when the original
  geometry is problematic or invalid. Set `TRUE` to remove these, based
  on whether a polygon fills up most of the x and y range.

## Value

data.table with combined spatial information

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

combineCellData(g, poly_info = "aggregate")
#> $rna
#> Key: <cell_ID>
#>                                        cell_ID    sdimx     sdimy  geom  part
#>                                         <char>    <num>     <num> <int> <num>
#>     1: 100210519278873141813371229408401071444 6637.881 -5140.465     1     1
#>     2: 100210519278873141813371229408401071444 6637.881 -5140.465     1     1
#>     3: 100210519278873141813371229408401071444 6637.881 -5140.465     1     1
#>     4: 100210519278873141813371229408401071444 6637.881 -5140.465     1     1
#>     5: 100210519278873141813371229408401071444 6637.881 -5140.465     1     1
#>    ---                                                                       
#> 35971:  98561957902191275233320065611022298397 6784.848 -4942.076   462     1
#> 35972:  98561957902191275233320065611022298397 6784.848 -4942.076   462     1
#> 35973:  98561957902191275233320065611022298397 6784.848 -4942.076   462     1
#> 35974:  98561957902191275233320065611022298397 6784.848 -4942.076   462     1
#> 35975:  98561957902191275233320065611022298397 6784.848 -4942.076   462     1
#>               x         y  hole  stack agg_n valid nr_feats perc_feats
#>           <num>     <num> <num> <char> <num> <int>    <int>      <num>
#>     1: 6642.257 -5136.674     0   <NA>     2     1       22    6.52819
#>     2: 6642.711 -5137.020     0   <NA>     2     1       22    6.52819
#>     3: 6643.050 -5137.462     0   <NA>     2     1       22    6.52819
#>     4: 6643.310 -5137.956     0   <NA>     2     1       22    6.52819
#>     5: 6643.484 -5138.518     0   <NA>     2     1       22    6.52819
#>    ---                                                                
#> 35971: 6788.764 -4942.998     0   <NA>     2     1       53   15.72700
#> 35972: 6788.905 -4942.999     0   <NA>     2     1       53   15.72700
#> 35973: 6789.575 -4942.999     0   <NA>     2     1       53   15.72700
#> 35974: 6789.576 -4943.000     0   <NA>     2     1       53   15.72700
#> 35975: 6789.695 -4943.000     0   <NA>     2     1       53   15.72700
#>        total_expr leiden_clus louvain_clus   feat
#>             <num>       <num>        <num> <char>
#>     1:   163.6923           4           15    rna
#>     2:   163.6923           4           15    rna
#>     3:   163.6923           4           15    rna
#>     4:   163.6923           4           15    rna
#>     5:   163.6923           4           15    rna
#>    ---                                           
#> 35971:   326.0539           1           26    rna
#> 35972:   326.0539           1           26    rna
#> 35973:   326.0539           1           26    rna
#> 35974:   326.0539           1           26    rna
#> 35975:   326.0539           1           26    rna
#> 
```
