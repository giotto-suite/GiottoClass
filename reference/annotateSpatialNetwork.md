# annotateSpatialNetwork

Annotate spatial network with cell metadata information.

## Usage

``` r
annotateSpatialNetwork(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spatial_network_name = "Delaunay_network",
  cluster_column,
  create_full_network = FALSE
)
```

## Arguments

- gobject:

  giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spatial_network_name:

  name of spatial network to use

- cluster_column:

  name of column to use for clusters

- create_full_network:

  convert from reduced to full network representation

## Value

annotated network in data.table format

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

annotateSpatialNetwork(g, cluster_column = "leiden_clus")
#>                     from                 to sdimx_begin sdimy_begin sdimx_end
#>                   <char>             <char>       <num>       <num>     <num>
#>    1: AAAGGGATGTAGCAAG-1 TCAAACAACCGCGTCG-1        5477       -4125      5340
#>    2: AAAGGGATGTAGCAAG-1 ACGATCATACATAGAG-1        5477       -4125      5546
#>    3: AAAGGGATGTAGCAAG-1 TATGCTCCCTACTTAC-1        5477       -4125      5408
#>    4: AAAGGGATGTAGCAAG-1 TTGTTCAGTGTGCTAC-1        5477       -4125      5615
#>    5: AAAGGGATGTAGCAAG-1 ATCGACTCTTTCCGTT-1        5477       -4125      5408
#>   ---                                                                        
#> 1766: TTCAAGCCGAGCTGAG-1 TTGTATCACACAGAAT-1        6372       -2808      6303
#> 1767: TTCGACGGGAAGGGCG-1 TTCGCACTCGCGTGCT-1        4239       -4125      4308
#> 1768: TTCTTAGTGGCTCAGA-1 TTGTGGCCCTGACAGT-1        5408       -3287      5340
#> 1769: TTCTTGTAACCTAATG-1 TTGGCTCGCATGAGAC-1        3620       -4005      3757
#> 1770: TTGCACGGAGCAGCAC-1 TTGTCGTTCAGTTACC-1        5271       -3766      5202
#>       sdimy_end distance      weight to_cell_type from_cell_type type_int
#>           <num>    <num>       <num>       <char>         <char>   <char>
#>    1:     -4125 137.0000 0.007299270            5              1   hetero
#>    2:     -4244 137.5573 0.007269700            1              1     homo
#>    3:     -4244 137.5573 0.007269700            1              1     homo
#>    4:     -4125 138.0000 0.007246377            1              1     homo
#>    5:     -4005 138.4233 0.007224219            1              1     homo
#>   ---                                                                    
#> 1766:     -2688 138.4233 0.007224219            2              2     homo
#> 1767:     -4245 138.4233 0.007224219            3              3     homo
#> 1768:     -3406 137.0584 0.007296161            5              5     homo
#> 1769:     -4005 137.0000 0.007299270            1              1     homo
#> 1770:     -3885 137.5573 0.007269700            1              3   hetero
#>       from_to unified_int
#>        <char>      <char>
#>    1:     1-5        1--5
#>    2:     1-1        1--1
#>    3:     1-1        1--1
#>    4:     1-1        1--1
#>    5:     1-1        1--1
#>   ---                    
#> 1766:     2-2        2--2
#> 1767:     3-3        3--3
#> 1768:     5-5        5--5
#> 1769:     1-1        1--1
#> 1770:     3-1        1--3
```
