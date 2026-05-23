# Overview

## Spatial unit and feature type

``` r

library(GiottoClass)

vizmini <- GiottoData::loadGiottoMini("vizgen")
```

    ## 1. read Giotto object

    ## 2. read Giotto feature information

    ## 3. read Giotto spatial information

    ## 4. read Giotto image information

    ## checking default envname 'giotto_env'

    ## a system default python environment was found

    ## Using python path:
    ##  "/usr/share/miniconda/envs/giotto_env/bin/python"

``` r

vizmini
```

    ## An object of class giotto 
    ## >Active spat_unit:  z0 
    ## >Active feat_type:  rna 
    ## dimensions    : 337, 498 (features, cells)
    ## [SUBCELLULAR INFO]
    ## polygons      : z0 z1 aggregate 
    ## features      : rna 
    ## [AGGREGATE INFO]
    ## expression -----------------------
    ##   [z0][rna] raw
    ##   [z1][rna] raw
    ##   [aggregate][rna] raw normalized scaled pearson
    ## spatial locations ----------------
    ##   [z0] raw
    ##   [z1] raw
    ##   [aggregate] raw
    ## spatial networks -----------------
    ##   [aggregate] Delaunay_network kNN_network
    ## spatial enrichments --------------
    ##   [aggregate][rna] cluster_metagene
    ## dim reduction --------------------
    ##   [aggregate][rna] pca umap tsne
    ## nearest neighbor networks --------
    ##   [aggregate][rna] sNN.pca
    ## attached images ------------------
    ## images      : 4 items...
    ## 
    ## 
    ## Use objHistory() to see steps and params used

## Show and list functions

Show and list functions are also provided for determining what
information exists within each of these slots and its nesting.

- show functions print a preview of all the data within the slot, but do
  not return information

``` r

showGiottoSpatLocs(vizmini)
```

    ## ├──Spatial unit "z0"
    ## │  └──S4 spatLocsObj "raw" coordinates:   (498 rows)
    ## │        An object of class spatLocsObj 
    ## │        provenance: z0
    ## │            ------------------------
    ## │              sdimx     sdimy                                 cell_ID
    ## │              <num>     <num>                                  <char>
    ## │        1: 6405.067 -4780.499  40951783403982682273285375368232495429
    ## │        2: 6426.020 -4972.519 240649020551054330404932383065726870513
    ## │        3: 6428.456 -4799.158 274176126496863898679934791272921588227
    ## │        4: 6408.155 -4816.583 323754550002953984063006506310071917306
    ## │        
    ## │        ranges:
    ## │                sdimx     sdimy
    ## │        [1,] 6402.438 -5146.726
    ## │        [2,] 6899.203 -4700.157
    ## │        
    ## │        
    ## │     
    ## ├──Spatial unit "z1"
    ## │  └──S4 spatLocsObj "raw" coordinates:   (504 rows)
    ## │        An object of class spatLocsObj 
    ## │        provenance: z1
    ## │            ------------------------
    ## │              sdimx     sdimy                                 cell_ID
    ## │              <num>     <num>                                  <char>
    ## │        1: 6404.014 -4779.625  40951783403982682273285375368232495429
    ## │        2: 6408.296 -4970.794  17685062374745280598492217386845129350
    ## │        3: 6401.148 -4991.061 223553142498364321238189328942498473503
    ## │        4: 6430.153 -4971.251 240649020551054330404932383065726870513
    ## │        
    ## │        ranges:
    ## │                sdimx     sdimy
    ## │        [1,] 6401.148 -5147.193
    ## │        [2,] 6899.323 -4700.410
    ## │        
    ## │        
    ## │     
    ## └──Spatial unit "aggregate"
    ##    └──S4 spatLocsObj "raw" coordinates:   (462 rows)
    ##          An object of class spatLocsObj 
    ##          provenance: z0 z1
    ##              ------------------------
    ##                sdimx     sdimy                                 cell_ID
    ##                <num>     <num>                                  <char>
    ##          1: 6637.881 -5140.465 100210519278873141813371229408401071444
    ##          2: 6471.978 -4883.541 101161259912191124732236989250178928032
    ##          3: 6801.610 -4968.685 101488859781016188084173008420811094152
    ##          4: 6789.055 -5105.338 101523780333017320796881555775415156847
    ##          
    ##          ranges:
    ##                  sdimx     sdimy
    ##          [1,] 6401.412 -5146.747
    ##          [2,] 6899.108 -4700.326
    ##          
    ##          
    ## 

``` r

list_expression(vizmini)
```

    ##    spat_unit feat_type       name
    ##       <char>    <char>     <char>
    ## 1:        z0       rna        raw
    ## 2:        z1       rna        raw
    ## 3: aggregate       rna        raw
    ## 4: aggregate       rna normalized
    ## 5: aggregate       rna     scaled
    ## 6: aggregate       rna    pearson

``` r

# Find specific spat_unit objects #
list_expression(vizmini, spat_unit = "z0")
```

    ##    spat_unit feat_type   name
    ##       <char>    <char> <char>
    ## 1:        z0       rna    raw

``` r

list_expression_names(vizmini, spat_unit = "z1", feat_type = "rna")
```

    ## [1] "raw"

## Provenance

Going further, sometimes different sources of information can be used
when aggregating to a particular spatial unit. This is most easily shown
with the subcellular datasets from the Vizgen MERSCOPE platform which
provide both feature polygon information for multiple confocal planes
within a tissue. The aggregated information produced then could be drawn
from different z-planes or combinations thereof. Giotto tracks this
provenance information for each set of aggregated data.

``` r

expr_mat <- getExpression(vizmini, spat_unit = "aggregate")
prov(expr_mat)
```

    ## [1] "z0" "z1"

## Giotto subobjects

Giotto 3.0 update introduced S4 subobjects that are used within the
giotto object and its processing. These subobjects provide more
formalized definitions for what information and formatting is needed in
each of the giotto object slots in order for it to be functional. These
objects are standalone and extensible and commonly used spatial
manipulation and plotting methods are being implemented for them.

In addition, these subobjects carry several pieces of metadata in
additional slots alongside the main information (e.g. also slots for
spat_unit and feat_type alongside the exprDT slot for the exprObj S4).
This makes it so that nesting information is retained when they are
taken out of the giotto object and that nesting information does not
need to be supplied anymore when interacting with the setter functions.

getter functions now have an output param that defaults to extracting
the information from the giotto object as the S4 subobject. When
extracting information that will be modified and then returned to the
giotto object, it is preferred that the information is extracted as the
S4 both so that tagged information is not lost, and because it is
convenient to work with the S4’s main data slot through the \[ and \[\<-
generics.

## Constructors

For directly creating a subobject, constructor functions can be used.

constructors

createExprObj() createCellMetaObj() createFeatMetaObj() createDimObj()
createNearestNetObj() createSpatLocsObj() createSpatNetObj()
createSpatEnrObj() createSpatialGrid() createGiottoPoints()
createGiottoPolygonsFromDfr() createGiottoPolygonsFromMask()
createGiottoImage() createGiottoLargeImage()

``` r

coords <- data.table::data.table(
    sdimx = c(1, 2, 3),
    sdimy = c(1, 2, 3),
    cell_ID = c("A", "B", "C")
)

st <- createSpatLocsObj(
    name = "test",
    spat_unit = "cell",
    coordinates = coords,
    provenance = "cell"
)
```

There are non numeric or integer columns for the spatial location input
at column position(s): 3 The first non-numeric column will be considered
as a cell ID to test for consistency with the expression matrix. Other
non numeric columns will be removed

``` r

print(st)
```

    ## An object of class spatLocsObj : "test"
    ## spat_unit : "cell"
    ## provenance: cell 
    ## dimensions: 3 3 
    ## preview   :
    ##    sdimx sdimy cell_ID
    ##    <num> <num>  <char>
    ## 1:     1     1       A
    ## 2:     2     2       B
    ## 3:     3     3       C
    ## 
    ## ranges:
    ##      sdimx sdimy
    ## [1,]     1     1
    ## [2,]     3     3

## Readers

Alternatively, read functions can be used to take named nested lists of
raw data input and convert them to lists of subobjects which are
directly usable by the setter functions.

readers

readPolygonData() readFeatData() readExprData() readCellMetadata()
readFeatMetadata() readSpatLocsData() readSpatNetData()
readSpatEnrichData() readDimReducData() readNearestNetData()

``` r

st2 <- readSpatLocsData(list(cell2 = list(
    test1 = coords,
    test2 = coords
)))
```

    ## list depth of 2

    ## 
    ## List item [1]:
    ##  spat_unit: cell2
    ##  name: test1

    ## 
    ## List item [2]:
    ##  spat_unit: cell2
    ##  name: test2

There are non numeric or integer columns for the spatial location input
at column position(s): 3. The first non-numeric column will be
considered as a cell ID to test for consistency with the expression
matrix. Other non numeric columns will be removed

There are non numeric or integer columns for the spatial location input
at column position(s): 3. The first non-numeric column will be
considered as a cell ID to test for consistency with the expression
matrix. Other non numeric columns will be removed

``` r

print(st2)
```

    ## [[1]]
    ## An object of class spatLocsObj : "test1"
    ## spat_unit : "cell2"
    ## provenance: cell2 
    ## dimensions: 3 3 
    ## preview   :
    ##    sdimx sdimy cell_ID
    ##    <num> <num>  <char>
    ## 1:     1     1       A
    ## 2:     2     2       B
    ## 3:     3     3       C
    ## 
    ## ranges:
    ##      sdimx sdimy
    ## [1,]     1     1
    ## [2,]     3     3
    ## 
    ## 
    ## [[2]]
    ## An object of class spatLocsObj : "test2"
    ## spat_unit : "cell2"
    ## provenance: cell2 
    ## dimensions: 3 3 
    ## preview   :
    ##    sdimx sdimy cell_ID
    ##    <num> <num>  <char>
    ## 1:     1     1       A
    ## 2:     2     2       B
    ## 3:     3     3       C
    ## 
    ## ranges:
    ##      sdimx sdimy
    ## [1,]     1     1
    ## [2,]     3     3

## Giotto Accessors

Giotto provides getter and setter functions for manually accessing the
information contained within the giotto object. Note that the setters
require that the data be provided as compatible S4 subobjects or lists
thereof. External data can read into the appropriate formats using the
above reader functions. The getter functions return S4 subobjects by
default.

getters

getExpression() getCellMetadata() getFeatMetadata()
getSpatialLocations() getDimReduction() getNearestNetwork()
getSpatialNetwork() getPolygonInfo() getFeatureInfo()
getSpatialEnrichment() getGiottoImage()

setters

setExpression() setCellMetadata() setFeatureMetadata()
setSpatialLocations() setDimReduction() setNearestNetwork()
setSpatialNetwork() setPolygonInfo() setFeatureInfo()
setSpatialEnrichment() setGiottoImage()

``` r

expval <- getExpression(vizmini)
print(expval)
```

    ## An object of class exprObj : "raw"
    ## spat_unit : "z0"
    ## feat_type : "rna"
    ## provenance: z0 
    ## 
    ## contains:
    ## 337 x 498 sparse Matrix of class "dgCMatrix"
    ##                                         
    ## Mlc1   . . . . . . . .  . . . . . ......
    ## Gprc5b . . 1 . 1 . . .  1 . 2 . . ......
    ## Gfap   . . . 1 1 . . . 27 . . . . ......
    ## 
    ##  ........suppressing 485 columns and 331 rows 
    ##                                           
    ## Adgrf4    . . . . . . . . . . . . . ......
    ## Epha2     . . . . . . . . . . . . . ......
    ## Blank-139 . . . . . . . . . . . . . ......
    ## 
    ##  First four colnames:
    ##  40951783403982682273285375368232495429
    ##  240649020551054330404932383065726870513
    ##  274176126496863898679934791272921588227
    ##  323754550002953984063006506310071917306

## Get and set S4 spat_unit, feat_type, provenance

spatUnit(), featType(), and prov() are replacement functions for tagged
spatial unit, feature type, and provenance information respectively.

``` r

# spat_unit
spatUnit(expval) <- "new_spat"
spatUnit(expval)
```

    ## [1] "new_spat"

``` r

# feat_type
featType(expval) <- "new_feat"
featType(expval)
```

    ## [1] "new_feat"

``` r

# provenance
prov(expval) <- "cell"
prov(expval)
```

    ## [1] "cell"

## Setting an S4 subobject

The spat_unit, feat_type, and name params no longer need to be given
when setting an S4 subobject with tagged information into a
giottoObject. However, if input is given to the set function parameters
then it is prioritized over the tagged information and the tagged
information is updated.

``` r

# set exprObj to tagged nesting location
vizmini <- setExpression(vizmini, expval)
```

    ## Setting expression [new_spat][new_feat] raw

``` r

list_expression(vizmini)
```

    ##    spat_unit feat_type       name
    ##       <char>    <char>     <char>
    ## 1:        z0       rna        raw
    ## 2:        z1       rna        raw
    ## 3: aggregate       rna        raw
    ## 4: aggregate       rna normalized
    ## 5: aggregate       rna     scaled
    ## 6: aggregate       rna    pearson
    ## 7:  new_spat  new_feat        raw

## Working with S4 subobjects

Giotto’s S4 subobjects each wrap one main data object. The empty \[\]
and \[\]\<- operators are defined as shorthand for directly accessing
this slot that contains the data. For example, with a spatLocsObj:

class(spatLocsObj\[\]) is equivalent to class(<spatLocsObj@coordinates>)

In this way, the S4 subobjects can be used in contexts that the wrapped
objects could be.

``` r

st <- getSpatialLocations(vizmini)
class(st)
```

    ## [1] "spatLocsObj"
    ## attr(,"package")
    ## [1] "GiottoClass"

``` r

# With empty brackets
class(st[])
```

    ## [1] "data.table" "data.frame"

### Setting information

``` r

print(st)
```

    ## An object of class spatLocsObj : "raw"
    ## spat_unit : "z0"
    ## provenance: z0 
    ## dimensions: 498 3 
    ## preview   :
    ##       sdimx     sdimy                                 cell_ID
    ##       <num>     <num>                                  <char>
    ## 1: 6405.067 -4780.499  40951783403982682273285375368232495429
    ## 2: 6426.020 -4972.519 240649020551054330404932383065726870513
    ## 3: 6428.456 -4799.158 274176126496863898679934791272921588227
    ## 
    ## ranges:
    ##         sdimx     sdimy
    ## [1,] 6402.438 -5146.726
    ## [2,] 6899.203 -4700.157

``` r

st[] <- coords
print(st)
```

    ## An object of class spatLocsObj : "raw"
    ## spat_unit : "z0"
    ## provenance: z0 
    ## dimensions: 3 3 
    ## preview   :
    ##    sdimx sdimy cell_ID
    ##    <num> <num>  <char>
    ## 1:     1     1       A
    ## 2:     2     2       B
    ## 3:     3     3       C
    ## 
    ## ranges:
    ##      sdimx sdimy
    ## [1,]     1     1
    ## [2,]     3     3

``` r

sessionInfo()
```

    ## R version 4.6.0 (2026-04-24)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] GiottoClass_0.5.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] rappdirs_0.3.4              sass_0.4.10                
    ##  [3] generics_0.1.4              SparseArray_1.12.2         
    ##  [5] gtools_3.9.5                lattice_0.22-9             
    ##  [7] digest_0.6.39               magrittr_2.0.5             
    ##  [9] evaluate_1.0.5              grid_4.6.0                 
    ## [11] fastmap_1.2.0               jsonlite_2.0.0             
    ## [13] Matrix_1.7-5                backports_1.5.1            
    ## [15] GiottoData_0.2.16           SingleCellExperiment_1.34.0
    ## [17] codetools_0.2-20            textshaping_1.0.5          
    ## [19] jquerylib_0.1.4             abind_1.4-8                
    ## [21] cli_3.6.6                   rlang_1.2.0                
    ## [23] XVector_0.52.0              Biobase_2.72.0             
    ## [25] withr_3.0.2                 cachem_1.1.0               
    ## [27] DelayedArray_0.38.1         yaml_2.3.12                
    ## [29] otel_0.2.0                  S4Arrays_1.12.0            
    ## [31] tools_4.6.0                 GiottoUtils_0.2.5          
    ## [33] checkmate_2.3.4             SpatialExperiment_1.22.0   
    ## [35] SummarizedExperiment_1.42.0 BiocGenerics_0.58.1        
    ## [37] reticulate_1.46.0           png_0.1-9                  
    ## [39] R6_2.6.1                    magick_2.9.1               
    ## [41] matrixStats_1.5.0           stats4_4.6.0               
    ## [43] lifecycle_1.0.5             Seqinfo_1.2.0              
    ## [45] S4Vectors_0.50.1            fs_2.1.0                   
    ## [47] htmlwidgets_1.6.4           IRanges_2.46.0             
    ## [49] ragg_1.5.2                  pkgconfig_2.0.3            
    ## [51] desc_1.4.3                  pkgdown_2.2.0              
    ## [53] terra_1.9-27                bslib_0.11.0               
    ## [55] data.table_1.18.4           Rcpp_1.1.1-1.1             
    ## [57] systemfonts_1.3.2           xfun_0.57                  
    ## [59] GenomicRanges_1.64.0        MatrixGenerics_1.24.0      
    ## [61] knitr_1.51                  rjson_0.2.23               
    ## [63] igraph_2.3.1                htmltools_0.5.9            
    ## [65] rmarkdown_2.31              compiler_4.6.0
