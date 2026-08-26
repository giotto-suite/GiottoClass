# createMetafeats

This function creates an average metafeat/metagene/module for clusters.

## Usage

``` r
createMetafeats(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  feat_clusters,
  stat = c("mean", "sum", "max", "min"),
  rescale_to = NULL,
  name = paste0("metafeat_", ifelse(is.function(stat), "custom", stat)),
  return_gobject = TRUE,
  verbose = NULL
)
```

## Arguments

- gobject:

  Giotto object

- spat_unit:

  spatial unit

- feat_type:

  feature type

- expression_values:

  expression values to use

- feat_clusters:

  numerical vector with features as names

- stat:

  statistical transformation to calculate across the cluster features
  per cell to get the metafeature value. Provided defaults are "mean",
  'sum', "max", and "min". If a function is supplied instead, it will be
  run via `apply(MARGIN = 2)` across the expression matrix to calculate
  the metafeature value.

- rescale_to:

  numeric vector of length 2. (optional) New maximum and minimum to
  rescale all features to before calculating the metafeature score using
  `stat`. Values are first zeroed by setting the minimum value to 0,
  then they are rescaled. In cases where all values are the same across
  a feature, the rescale produces the middle point of the range
  specified by `rescale_to`

- name:

  name of the metagene results

- return_gobject:

  return giotto object

- verbose:

  be verbose

## Value

giotto object

## Details

An example for the 'gene_clusters' could be like this: cluster_vector =
c(1, 1, 2, 2) names(cluster_vector) = c('geneA', 'geneB', 'geneC',
'geneD')

## Examples

``` r
# load a dataset
g <- GiottoData::loadGiottoMini("viz")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
# set a spat unit to use
activeSpatUnit(g) <- "aggregate"

# define the metafeats
# We do this by making a set of annotations. These annotations can be
# made as either a vector or data.frame

# 1.1 annotation vector (numeric)
feats_to_use <- featIDs(g)[seq_len(6)]
clust_to_use1 <- c(1, 1, 1, 2, 2, 2)
names(clust_to_use1) <- feats_to_use
force(clust_to_use1)
#>   Mlc1 Gprc5b   Gfap  Ednrb   Sox9   Aqp4 
#>      1      1      1      2      2      2 

g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use1,
    name = "metagene_1"
)
#> calculating metafeature: 1
#> calculating metafeature: 2

# 1.2 annotation vector (character)
clust_to_use2 <- c(rep("sig_a", 3), rep("sig_b", 3))
names(clust_to_use2) <- feats_to_use
force(clust_to_use2)
#>    Mlc1  Gprc5b    Gfap   Ednrb    Sox9    Aqp4 
#> "sig_a" "sig_a" "sig_a" "sig_b" "sig_b" "sig_b" 

g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use2,
    name = "metagene_2"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b

# 2.1 annotation data.frame (no weights)
# Cols must be:
# `clus` - which metafeature
# `feat` - features to assign to this metafeature
# `w` - (optional) weight to assign the feature before metafeature score
#       calculation
clust_to_use3 <- data.frame(
    clus = c(rep("sig_a", 3), rep("sig_b", 3)),
    feat = feats_to_use
)
force(clust_to_use3)
#>    clus   feat
#> 1 sig_a   Mlc1
#> 2 sig_a Gprc5b
#> 3 sig_a   Gfap
#> 4 sig_b  Ednrb
#> 5 sig_b   Sox9
#> 6 sig_b   Aqp4
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use3,
    name = "metagene_3"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b

# 2.1 annotation data.frame (weights)
clust_to_use4 <- data.frame(
    clus = c(rep("sig_a", 3), rep("sig_b", 3)),
    feat = feats_to_use,
    w = c(1, 1, 1, 2, 3, 4)
)
force(clust_to_use4)
#>    clus   feat w
#> 1 sig_a   Mlc1 1
#> 2 sig_a Gprc5b 1
#> 3 sig_a   Gfap 1
#> 4 sig_b  Ednrb 2
#> 5 sig_b   Sox9 3
#> 6 sig_b   Aqp4 4
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    name = "weighted_metagene"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b

# 3. Other statistic transforms can be calculated
# The default and what has been used so far is just finding the mean
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    stat = "sum",
    expression_values = "raw", # find summed raw expr for these feats
    name = "raw_sums"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b

# 4. A custom stat function can also be supplied
# These custom functions must be summary functions, as in, they must
# produce only a single numeric output from many
custom_stat <- function(x) {
    if (max(x) == 0) {
        return(0)
    }
    return(mean(x / max(x)))
}
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    stat = custom_stat,
    expression_values = "raw",
    name = "raw_custom"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b

# 5. A rescale of the values to a specified numeric range can be applied
# before the stat score calculation.
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    stat = "mean",
    expression_values = "normalized",
    rescale_to = c(0, 1),
    name = "norm_scaled_mean_metafeat"
)
#> calculating metafeature: sig_a
#> calculating metafeature: sig_b
showGiottoSpatEnrichments(g)
#> Spatial unit:  aggregate  
#> 
#> --> Feature type:  rna  
#> 
#> ----> Name  cluster_metagene : 
#>           1         2         3         4     5
#>       <num>     <num>     <num>     <num> <num>
#> 1: 1.014837 0.0000000 0.3160792 0.0000000     0
#> 2: 3.207415 0.9579716 0.6728505 0.2132677     0
#> 3: 3.953661 0.4604975 0.0000000 0.2302488     0
#> 4: 4.093622 0.5886051 0.1962017 0.5886051     0
#>                                    cell_ID
#>                                     <char>
#> 1: 240649020551054330404932383065726870513
#> 2: 274176126496863898679934791272921588227
#> 3: 323754550002953984063006506310071917306
#> 4:  87260224659312905497866017323180367450
#> 
#> ----> Name  metagene_1 : 
#>           1        2                                 cell_ID
#>       <num>    <num>                                  <char>
#> 1: 3.493789 3.160792 240649020551054330404932383065726870513
#> 2: 2.132677 2.795048 274176126496863898679934791272921588227
#> 3: 2.633814 7.570115 323754550002953984063006506310071917306
#> 4: 4.446903 3.924034  87260224659312905497866017323180367450
#> 
#> ----> Name  metagene_2 : 
#>       sig_a    sig_b                                 cell_ID
#>       <num>    <num>                                  <char>
#> 1: 3.493789 3.160792 240649020551054330404932383065726870513
#> 2: 2.132677 2.795048 274176126496863898679934791272921588227
#> 3: 2.633814 7.570115 323754550002953984063006506310071917306
#> 4: 4.446903 3.924034  87260224659312905497866017323180367450
#> 
#> ----> Name  metagene_3 : 
#>       sig_a    sig_b                                 cell_ID
#>       <num>    <num>                                  <char>
#> 1: 3.493789 3.160792 240649020551054330404932383065726870513
#> 2: 2.132677 2.795048 274176126496863898679934791272921588227
#> 3: 2.633814 7.570115 323754550002953984063006506310071917306
#> 4: 4.446903 3.924034  87260224659312905497866017323180367450
#> 
#> ----> Name  weighted_metagene : 
#>       sig_a     sig_b                                 cell_ID
#>       <num>     <num>                                  <char>
#> 1: 3.493789  6.321584 240649020551054330404932383065726870513
#> 2: 2.132677  5.590096 274176126496863898679934791272921588227
#> 3: 2.633814 23.041672 323754550002953984063006506310071917306
#> 4: 4.446903 11.772102  87260224659312905497866017323180367450
#> 
#> ----> Name  raw_sums : 
#>    sig_a sig_b                                 cell_ID
#>    <num> <num>                                  <char>
#> 1:     2     2 240649020551054330404932383065726870513
#> 2:     1     8 274176126496863898679934791272921588227
#> 3:     2    16 323754550002953984063006506310071917306
#> 4:     4     6  87260224659312905497866017323180367450
#> 
#> ----> Name  raw_custom : 
#>        sig_a     sig_b                                 cell_ID
#>        <num>     <num>                                  <char>
#> 1: 0.3333333 0.3333333 240649020551054330404932383065726870513
#> 2: 0.3333333 0.3333333 274176126496863898679934791272921588227
#> 3: 0.3333333 0.6666667 323754550002953984063006506310071917306
#> 4: 0.4444444 0.5000000  87260224659312905497866017323180367450
#> 
#> ----> Name  norm_scaled_mean_metafeat : 
#>        sig_a     sig_b                                 cell_ID
#>        <num>     <num>                                  <char>
#> 1: 0.3009266 0.6666667 240649020551054330404932383065726870513
#> 2: 0.2139690 0.5895248 274176126496863898679934791272921588227
#> 3: 0.2268554 2.3591938 323754550002953984063006506310071917306
#> 4: 0.4182979 1.2189606  87260224659312905497866017323180367450
```
