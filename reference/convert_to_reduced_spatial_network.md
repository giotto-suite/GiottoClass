# convert_to_reduced_spatial_network

Convert to a reduced spatial network. Specifically, removes the
duplicated connections so that only \\a\\ -\> \\b\\ interactions remain.

## Usage

``` r
convert_to_reduced_spatial_network(full_spatial_network_DT)
```

## Arguments

- full_spatial_network_DT:

  full spatial network in data.table format

## Value

data.table

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
spat_net <- getSpatialNetwork(g, output = "networkDT")
spat_net_full <- convert_to_full_spatial_network(spat_net)

convert_to_reduced_spatial_network(spat_net_full)
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
#>       sdimy_end distance      weight
#>           <num>    <num>       <num>
#>    1:     -4125 137.0000 0.007299270
#>    2:     -4244 137.5573 0.007269700
#>    3:     -4244 137.5573 0.007269700
#>    4:     -4125 138.0000 0.007246377
#>    5:     -4005 138.4233 0.007224219
#>   ---                               
#> 1766:     -2688 138.4233 0.007224219
#> 1767:     -4245 138.4233 0.007224219
#> 1768:     -3406 137.0584 0.007296161
#> 1769:     -4005 137.0000 0.007299270
#> 1770:     -3885 137.5573 0.007269700
```
