# The object manifest

A `giotto` object is large, deeply nested, and mostly opaque from the
console. Two questions come up constantly and neither has a good answer
from printed output:

- **What is in this object?**
- **Did the step I just ran actually change anything?**

[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md)
answers the first and
[`manifestDiff()`](https://giotto-suite.github.io/GiottoClass/dev/reference/manifestDiff.md)
answers the second.

``` r

library(GiottoClass)

g <- GiottoData::loadGiottoMini("vizgen")
activeSpatUnit(g) <- "aggregate"

objManifest(g)
#> <gmanifest> schema 0.1.0
#>   uid: g-20260925200852-3e61352c3331
#>   spat_units: aggregate, z0, z1 | feat_types: rna
#>   cell_ID: 3
#>   cell_metadata: 3
#>   dimension_reduction: 3
#>   expression: 6
#>   feat_ID: 1
#>   feat_info: 1
#>   feat_metadata: 3
#>   images: 4
#>   nn_network: 1
#>   spatial_enrichment: 1
#>   spatial_info: 3
#>   spatial_locs: 3
#>   spatial_network: 2
```

The manifest is what the object **is**. Its companion,
[`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md),
records **why** it looks that way. The two are deliberately separate:
the history is provenance, and object state is never reconstructed by
replaying it.

## Reading the inventory

The manifest nests exactly as the object nests, so a path through it is
a path through the object — `spat_unit`, then `feat_type`, then name:

``` r

m <- objManifest(g)
str(m$slots$expression$aggregate$rna$raw)
#> List of 10
#>  $ class     : chr "exprObj"
#>  $ name      : chr "raw"
#>  $ spat_unit : chr "aggregate"
#>  $ feat_type : chr "rna"
#>  $ provenance: chr [1:2] "z0" "z1"
#>  $ shape     : int [1:2] 337 462
#>  $ dtype     : chr "dgCMatrix"
#>  $ sparse    : logi TRUE
#>  $ nnz       : int 15307
#>  $ density   : num 0.0983
```

Every accessor used to build a leaf is individually guarded. A field
that cannot be read becomes `NULL` and its path is listed in `warnings`,
rather than erroring the whole manifest — a state report that crashes is
worse than no state report. On a healthy object the list is empty:

``` r

m$warnings
#> character(0)
```

## Did my step do anything?

This is the day-to-day use. Take a manifest before the step, another
after, and ask what moved:

``` r

before <- objManifest(g, level = "full")

g <- createSpatialNetwork(g, name = "tutorial_net", method = "Delaunay")

manifestDiff(before, objManifest(g, level = "full"))$summary
#> [1] "spatial network added: tutorial_net"
```

Steps that overwrite something already present are the interesting case.
Recomputing an existing network leaves its name, its position and often
its shape untouched, and only the content changes:

``` r

before <- objManifest(g, level = "full")

g <- createNearestNetwork(g, name = "sNN.pca", k = 10)
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [aggregate][rna] sNN.pca
#> Setting nearest neighbor network [aggregate][rna] kNN.pca

manifestDiff(before, objManifest(g, level = "full"))$summary
#> [1] "NN network added: kNN.pca | NN network sNN.pca modified (n_edges, fingerprint)"
```

[`manifestDiff()`](https://giotto-suite.github.io/GiottoClass/dev/reference/manifestDiff.md)
also returns `changed` (a flag) and `detail` (every affected leaf, as
data). The one-sentence `summary` is what you would hand to a person, or
a model; `detail` is what you would score or assert against.

## `summary` versus `full`

`level = "summary"` describes structure: names, shapes, column names,
counts. `level = "full"` adds a content fingerprint per leaf. That
difference matters exactly when a step rewrites values in place:

``` r

g2 <- createNearestNetwork(g, name = "sNN.pca", k = 20)
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [aggregate][rna] sNN.pca
#> > 'kNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [aggregate][rna] kNN.pca

# structure only
manifestDiff(objManifest(g), objManifest(g2))$summary
#> [1] "NN network kNN.pca modified (n_edges) | NN network sNN.pca modified (n_edges)"

# structure and content
manifestDiff(objManifest(g, level = "full"),
             objManifest(g2, level = "full"))$summary
#> [1] "NN network kNN.pca modified (n_edges, fingerprint) | NN network sNN.pca modified (n_edges, fingerprint)"
```

Fingerprints are cheaper than they sound. They hash a deterministic
fixed-stride sample of the content rather than all of it, so the cost is
flat in object size — about 6 ms for a two-million-point `giottoPoints`
and 1 ms for a matrix with twenty million non-zeros. A full manifest of
a real Xenium crop takes ~44 ms and occupies ~120 KB in memory.

Sampling is stride-based rather than random, so it never touches the RNG
and never disturbs a seeded analysis.

A fingerprint identifies *values*, not their storage. Numbers are
compared to 12 significant digits and geometry coordinates as an ordered
set of vertices, so reading back an object you just saved does not
report a change merely because a file format dropped the last bits of a
double or renumbered the vertices of a polygon. The same slack means a
difference smaller than about 1e-12 relative is not reported at all.

## The history log

[`ghistory_records()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ghistory_records.md)
returns the object’s history as structured records rather than printed
text:

``` r

g <- subsetGiotto(g, cell_ids = head(spatIDs(g), 100))

str(tail(ghistory_records(g), 1)[[1]], max.level = 1)
#> List of 8
#>  $ step_id  : chr "19_subset"
#>  $ fn       : chr "subsetGiotto"
#>  $ params   :List of 12
#>  $ timestamp: chr "2026-09-25T20:08:54Z"
#>  $ seed     :List of 2
#>  $ status   : chr "ok"
#>  $ error    : NULL
#>  $ diff     : NULL
```

`params` holds the arguments the function actually ran with, as deparsed
expressions, so `1:30` survives as `"1:30"`.

Not every change is logged, though. Only a minority of the suite’s
functions call
[`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md),
and a direct slot assignment calls nothing at all — so an object’s
contents can move with nothing in its history claiming responsibility.
[`recordGiottoStep()`](https://giotto-suite.github.io/GiottoClass/dev/reference/recordGiottoStep.md)
closes that gap for a caller that can see the change happen:

``` r

before <- objManifest(g)

cm <- getCellMetadata(g, output = "cellMetaObj")
cm[]$hand_label <- rep(c("tumor", "stroma"), length.out = nrow(cm))
g <- setGiotto(g, cm, verbose = FALSE)

d <- manifestDiff(before, objManifest(g))
d$summary
#> [1] "cell metadata added: hand_label (2 levels)"

g <- recordGiottoStep(g, fn = "manual annotation",
                      status = "unattributed", diff = d$detail)

tail(names(objHistory(g)), 1)
#> [1] "20_unattributed"
```

A failed call is recordable the same way, with `status = "error"`.

## On disk

[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
writes two sidecars beside the object: `manifest.json` (what it is) and
`history.ndjson` (why, one operation per line).

``` r

td <- file.path(tempdir(), "manifest_vignette")
dir.create(td, recursive = TRUE, showWarnings = FALSE)

saveGiotto(g, dir = td, foldername = "mini", verbose = FALSE)

saved <- file.path(td, "mini")
list.files(saved)
#> [1] "Features"       "gobject.RDS"    "history.ndjson" "Images"        
#> [5] "manifest.json"  "SpatialInfo"
```

The point of the sidecar is that it answers questions about the object
without loading it. Here the manifest is a few tens of kilobytes beside
a payload thousands of times larger, and the ratio only improves as
objects grow:

``` r

sc <- jsonlite::fromJSON(file.path(saved, "manifest.json"),
                         simplifyVector = FALSE)

unlist(sc$summary$spat_units)
#> [1] "aggregate" "z0"        "z1"
unlist(sc$summary$n_cells)
#>        z0        z1 aggregate 
#>       100       100       100
names(sc$slots$expression$aggregate$rna)
#> [1] "normalized" "pearson"    "raw"        "scaled"
```

[`objManifest_json()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest_json.md)
and
[`objHistory_ndjson()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory_ndjson.md)
produce the same two documents for any object, whether or not you are
saving it. The JSON follows a versioned schema that ships with the
package:

``` r

system.file("schema", "giotto-manifest-0.1.0.json", package = "GiottoClass")
#> [1] "/home/runner/work/_temp/Library/GiottoClass/schema/giotto-manifest-0.1.0.json"
```

Keys are sorted at every level so two manifests of the same state
serialize identically, and `NA`, `NaN` and `Inf` are encoded as strings
so that a missing value stays distinguishable from a not-a-number.

## Beyond GiottoClass

The same two documents appear elsewhere in the suite:

- **GiottoDisk** writes them beside each snapshot in a project
  directory. `snapshotManifest()` and `snapshotHistory()` read them
  back, so you can see what a snapshot holds without loading it.
- **GiottoAgent** attaches `status`, `compact_diff` and `manifest_ref`
  to every execution result, so an assistant driving an analysis reads
  what a step did instead of inferring it from console output.

## What it is good for

- Checking that a step did what you expected — and noticing when it did
  nothing.
- Inspecting a saved object or a snapshot without loading it.
- Regression-testing a pipeline: same inputs should give the same
  fingerprints.
- Diffing two runs, or the same analysis before and after a parameter
  change.
- Turning `history.ndjson` into a methods section.
- Giving an assistant, an eval harness or a reviewer machine-readable
  state instead of printed text.

## What it does not do

- **Disk-backed expression gets no fingerprint.** GiottoDisk stores,
  `DelayedArray` and BPCells would have to be materialised to hash, so
  they degrade to no fingerprint rather than pulling the data through
  memory. A change made in place to disk-backed values is therefore
  invisible to the diff.
- **The history is not complete by construction.** It records what
  functions chose to log. Entries written with `toplevel = 1` name
  `update_giotto_params` rather than the calling function — a property
  of those call sites, not of the record.
- **The schema is 0.1.0 and not yet released.** Treat the field set as
  stable in spirit, not frozen.
