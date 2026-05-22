# Subset `giotto` subobjects

Subset a `giotto` object with `[[` to disassemble it into a list of
Giotto S4 subobjects. If `drop` is `FALSE`, the selected subobjects will
be reassembled into a new `giotto` object. Note that indexing within the
`[[` filters for only those subobjects that have those attributes. This
may remove some unexpected information. For specifically splitting the
`giotto` object by spatial unit and/or feature type while keeping all
expected information, use
[`sliceGiotto()`](https://giotto-suite.github.io/GiottoClass/reference/sliceGiotto.md)

## Usage

``` r
# S4 method for class 'giotto,missing,missing'
x[[spat_unit = NULL, feat_type = NULL, drop = TRUE, ...]]

# S4 method for class 'giotto,character,missing'
x[[i, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...]]

# S4 method for class 'giotto,missing,character'
x[[j, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...]]

# S4 method for class 'giotto,character,character'
x[[i, j, spat_unit = NULL, feat_type = NULL, drop = TRUE, ...]]
```

## Arguments

- x:

  giotto object

- spat_unit:

  spatial unit (e.g. "cell")

- feat_type:

  feature type to use (e.g. "rna", "protein")

- drop:

  logical. Default = TRUE

- ...:

  additional arguments

- i:

  character. Indicates the slot name

- j:

  character. Indicates the subobject name

## Value

giotto subobject

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
force(g)
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : z0 z1 aggregate 
#> features      : rna 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> attached images ------------------
#> images      : 4 items...
#> 
#> 
#> Use objHistory() to see steps and params used

# return as lists of subobjects with drop = TRUE (default)
g[[, "raw"]]
#> [[1]]
#> An object of class spatLocsObj : "raw"
#> spat_unit : "z0"
#> provenance: z0 
#> dimensions: 498 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6405.067 -4780.499  40951783403982682273285375368232495429
#> 2: 6426.020 -4972.519 240649020551054330404932383065726870513
#> 3: 6428.456 -4799.158 274176126496863898679934791272921588227
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6402.438 -5146.726
#> [2,] 6899.203 -4700.157
#> 
#> 
#> [[2]]
#> An object of class spatLocsObj : "raw"
#> spat_unit : "z1"
#> provenance: z1 
#> dimensions: 504 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6404.014 -4779.625  40951783403982682273285375368232495429
#> 2: 6408.296 -4970.794  17685062374745280598492217386845129350
#> 3: 6401.148 -4991.061 223553142498364321238189328942498473503
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6401.148 -5147.193
#> [2,] 6899.323 -4700.410
#> 
#> 
#> [[3]]
#> An object of class spatLocsObj : "raw"
#> spat_unit : "aggregate"
#> provenance: z0 z1 
#> dimensions: 462 3 
#> preview   :
#>       sdimx     sdimy                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 6637.881 -5140.465 100210519278873141813371229408401071444
#> 2: 6471.978 -4883.541 101161259912191124732236989250178928032
#> 3: 6801.610 -4968.685 101488859781016188084173008420811094152
#> 
#> ranges:
#>         sdimx     sdimy
#> [1,] 6401.412 -5146.747
#> [2,] 6899.108 -4700.326
#> 
#> 
#> [[4]]
#> An object of class exprObj : "raw"
#> spat_unit : "z0"
#> feat_type : "rna"
#> provenance: z0 
#> 
#> contains:
#> 337 x 498 sparse Matrix of class "dgCMatrix"
#>                                         
#> Mlc1   . . . . . . . .  . . . . . ......
#> Gprc5b . . 1 . 1 . . .  1 . 2 . . ......
#> Gfap   . . . 1 1 . . . 27 . . . . ......
#> 
#>  ........suppressing 485 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  40951783403982682273285375368232495429
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306 
#> 
#> [[5]]
#> An object of class exprObj : "raw"
#> spat_unit : "z1"
#> feat_type : "rna"
#> provenance: z1 
#> 
#> contains:
#> 337 x 504 sparse Matrix of class "dgCMatrix"
#>                                         
#> Mlc1   . . . . . . . . . . .  1 . ......
#> Gprc5b . . . . . . . 2 . . .  1 . ......
#> Gfap   . 3 . 2 . 1 4 . . . . 18 . ......
#> 
#>  ........suppressing 491 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  40951783403982682273285375368232495429
#>  17685062374745280598492217386845129350
#>  223553142498364321238189328942498473503
#>  240649020551054330404932383065726870513 
#> 
#> [[6]]
#> An object of class exprObj : "raw"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 337 x 462 sparse Matrix of class "dgCMatrix"
#>                                         
#> Mlc1   . . . . . .  1 . . 1 . . . ......
#> Gprc5b . 1 . 3 . .  2 . 4 2 . 1 . ......
#> Gfap   2 . 2 1 . . 45 . 1 1 . . . ......
#> 
#>  ........suppressing 449 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
#> 
g[["expression", spat_unit = "aggregate"]]
#> [[1]]
#> An object of class exprObj : "raw"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 337 x 462 sparse Matrix of class "dgCMatrix"
#>                                         
#> Mlc1   . . . . . .  1 . . 1 . . . ......
#> Gprc5b . 1 . 3 . .  2 . 4 2 . 1 . ......
#> Gfap   2 . 2 1 . . 45 . 1 1 . . . ......
#> 
#>  ........suppressing 449 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
#> 
#> [[2]]
#> An object of class exprObj : "normalized"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 337 x 462 sparse Matrix of class "dgCMatrix"
#>                                                                                          
#> Mlc1    .       .        .        .        . .  5.291601 . .        5.821597 . .        .
#> Gprc5b  .       6.398031 .        7.454658 . .  6.273066 . 7.680054 6.808785 . 5.082989 .
#> Gfap   10.48137 .        7.901442 5.886051 . . 10.746982 . 5.701006 5.821597 . .        .
#>              
#> Mlc1   ......
#> Gprc5b ......
#> Gfap   ......
#> 
#>  ........suppressing 449 columns and 331 rows 
#>                                           
#> Adgrf4    . . . . . . . . . . . . . ......
#> Epha2     . . . . . . . . . . . . . ......
#> Blank-139 . . . . . . . . . . . . . ......
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
#> 
#> [[3]]
#> An object of class exprObj : "scaled"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 
#> 337 x 462 dense matrix of class "dgeMatrix"
#> 
#> Mlc1      -0.51096453 -0.4718074 -0.3859440 -0.4383384 -0.3827836 -0.3152653 ...... 
#> Gprc5b    -0.97549417  1.6804631 -0.5746594  1.2850731 -0.5794505 -0.4583322 ...... 
#> Gfap       5.46807822 -0.7367558  1.3902670  0.5804327 -0.6152080 -0.4843442 ...... 
#> Ednrb      9.34063752  3.7384966  2.5412669  1.5736461  2.8570134 -0.2823656 ...... 
#> Sox9      -0.59906517 -0.5143258  2.4318798 -0.4671338 -0.4200826 -0.3423988 ...... 
#> Aqp4      -0.64987710 -0.5388482  2.0705344  1.0223253  1.9919249 -0.3580479 ...... 
#> Gjc3       0.02982685  5.0161718 -0.1662473 -0.2615829 -0.1538300 -0.1487112 ...... 
#> Sox8      -0.47826316  3.5325216  2.5366322 -0.4276501 -0.3689389 -0.3051939 ...... 
#> Ntsr2      6.06580557  2.6068464  1.7367543  1.1330202 -0.6019376 -0.4746906 ...... 
#> Adcyap1r1 -2.17060870  0.8282834  0.8492637 -0.9807865 -1.0854235 -0.8264062 ...... 
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
#> 
#> [[4]]
#> An object of class exprObj : "pearson"
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: z0 z1 
#> 
#> contains:
#> 
#> 337 x 462 dense matrix of class "dgeMatrix"
#> 
#> Mlc1      -0.1671233 -0.4887705 -0.4090814 -0.5848627 -0.3515289 -0.3681237 ...... 
#> Gprc5b    -0.2390668  0.7264405 -0.5847568  2.7313611 -0.5026041 -0.5262978 ...... 
#> Gfap       4.1820021 -1.2576333  0.8204327 -0.8491028 -0.9074561 -0.9499730 ...... 
#> Ednrb      6.2681837  8.3133382  2.2399348  1.2854419  2.7238921 -0.3428383 ...... 
#> Sox9      -0.1862585 -0.5445941  3.9225159 -0.6515799 -0.3917326 -0.4102193 ...... 
#> Aqp4      -0.2836816 -0.8280158  2.1766489  0.0105117  1.0752709 -0.6242357 ...... 
#> Gjc3      -0.1409193  4.4308083 -0.3450090 -0.4933833 -0.2964520 -0.3104520 ...... 
#> Sox8      -0.1577216  6.0278079  2.2000617 -0.5520553 -0.3317706 -0.3474348 ...... 
#> Ntsr2      5.2211595 12.2321831  2.5314771  1.9147811 -0.7517658 -0.7870865 ...... 
#> Adcyap1r1 -0.3682081 -0.1508472  0.2046257 -1.2808439 -0.7730688 -0.8093776 ...... 
#> 
#>  First four colnames:
#>  240649020551054330404932383065726870513
#>  274176126496863898679934791272921588227
#>  323754550002953984063006506310071917306
#>  87260224659312905497866017323180367450 
#> 

# return as a subset giotto object with drop = FALSE
g[[, "raw", drop = FALSE]]
#> An object of class giotto 
#> >Active spat_unit:  z0 
#> >Active feat_type:  rna 
#> dimensions    : 337, 498 (features, cells)
#> [SUBCELLULAR INFO]
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [z0][rna] raw
#>   [z1][rna] raw
#>   [aggregate][rna] raw
#> spatial locations ----------------
#>   [z0] raw
#>   [z1] raw
#>   [aggregate] raw
#> 
#> 
#> Use objHistory() to see steps and params used
g[[spat_unit = "aggregate", drop = FALSE]]
#> An object of class giotto 
#> >Active spat_unit:  aggregate 
#> >Active feat_type:  rna 
#> dimensions    : 337, 462 (features, cells)
#> [SUBCELLULAR INFO]
#> polygons      : aggregate 
#> [AGGREGATE INFO]
#> expression -----------------------
#>   [aggregate][rna] raw normalized scaled pearson
#> spatial locations ----------------
#>   [aggregate] raw
#> spatial networks -----------------
#>   [aggregate] Delaunay_network kNN_network
#> spatial enrichments --------------
#>   [aggregate][rna] cluster_metagene
#> dim reduction --------------------
#>   [aggregate][rna] pca umap tsne
#> nearest neighbor networks --------
#>   [aggregate][rna] sNN.pca
#> 
#> 
#> Use objHistory() to see steps and params used
```
