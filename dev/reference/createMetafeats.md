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

g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use1,
    name = "metagene_1"
)

# 1.2 annotation vector (character)
clust_to_use2 <- c(rep("sig_a", 3), rep("sig_b", 3))
names(clust_to_use2) <- feats_to_use
force(clust_to_use2)

g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use2,
    name = "metagene_2"
)

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
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use3,
    name = "metagene_3"
)

# 2.1 annotation data.frame (weights)
clust_to_use4 <- data.frame(
    clus = c(rep("sig_a", 3), rep("sig_b", 3)),
    feat = feats_to_use,
    w = c(1, 1, 1, 2, 3, 4)
)
force(clust_to_use4)
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    name = "weighted_metagene"
)

# 3. Other statistic transforms can be calculated
# The default and what has been used so far is just finding the mean
g <- createMetafeats(
    gobject = g,
    feat_clusters = clust_to_use4,
    stat = "sum",
    expression_values = "raw", # find summed raw expr for these feats
    name = "raw_sums"
)

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
showGiottoSpatEnrichments(g)
```
