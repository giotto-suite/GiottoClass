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
#> <gmanifest> schema 0.2.0
#>   uid: g-20261022202917-405419191f55 | source: in memory
#>   cells: z0 498, z1 504, aggregate 462 | features: rna 337
#>   cell_metadata
#>     aggregate / rna
#>     z0 / rna
#>     z1 / rna
#>   dimension_reduction
#>     cells / aggregate / rna / pca / pca
#>     cells / aggregate / rna / tsne / tsne
#>     cells / aggregate / rna / umap / umap
#>   expression
#>     aggregate / rna / normalized
#>     aggregate / rna / pearson
#>     aggregate / rna / raw
#>     aggregate / rna / scaled
#>     z0 / rna / raw
#>     z1 / rna / raw
#>   feat_info
#>     rna
#>   feat_metadata
#>     aggregate / rna
#>     z0 / rna
#>     z1 / rna
#>   images
#>     dapi_z0
#>     dapi_z1
#>     polyT_z0
#>     polyT_z1
#>   nn_network
#>     aggregate / rna / sNN / sNN.pca
#>   spatial_enrichment
#>     aggregate / rna / cluster_metagene
#>   spatial_info
#>     aggregate
#>     z0
#>     z1
#>   spatial_locs
#>     aggregate / raw
#>     z0 / raw
#>     z1 / raw
#>   spatial_network
#>     aggregate / Delaunay_network
#>     aggregate / kNN_network
```

The manifest is what the object **is**. Its companion,
[`objHistory()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory.md),
records **why** it looks that way. The two are deliberately separate:
the history is provenance, and object state is never reconstructed by
replaying it.

## Reading the inventory

`slots` holds one entry per non-empty slot, and each entry is a list of
records, one per subobject. A record’s identity is under `keys`: the
accessor arguments that retrieve it, and only the ones that apply.
Everything else describes it: its class, its form, and a fingerprint of
its content.

``` r

m <- objManifest(g)
str(m$slots$expression[[1]])
#> List of 5
#>  $ keys       :List of 3
#>   ..$ spat_unit: chr "aggregate"
#>   ..$ feat_type: chr "rna"
#>   ..$ name     : chr "normalized"
#>  $ class      : chr "exprObj"
#>  $ provenance : chr [1:2] "z0" "z1"
#>  $ dtype      : chr "dgCMatrix"
#>  $ fingerprint: chr "933e89d45e719a03b8c2231de99fc524"
```

Because the keys are the getter’s own arguments, a record maps straight
onto a call:

``` r

rec <- m$slots$dimension_reduction[[1]]
rec$keys
#> $reduction
#> [1] "cells"
#> 
#> $spat_unit
#> [1] "aggregate"
#> 
#> $feat_type
#> [1] "rna"
#> 
#> $reduction_method
#> [1] "pca"
#> 
#> $name
#> [1] "pca"

pca <- do.call(getDimReduction, c(list(g), rec$keys, list(output = "dimObj")))
objName(pca)
#> [1] "pca"
```

Records report form and content, not size. Dimensions, row counts and
extents are deliberately absent: on a disk-backed object each one costs
a scan of the data. The counts that matter for reading a diff are in the
`summary` block, which comes from the object’s id slots for free:

``` r

unlist(m$summary$n_cells)
#>        z0        z1 aggregate 
#>       498       504       462
```

Every field is read through its own guard. A field that cannot be read
becomes `NULL` and its path is listed in `warnings`, rather than
erroring the whole manifest: a state report that crashes is worse than
no state report. On a healthy object the list is empty:

``` r

m$warnings
#> character(0)
```

## Did my step do anything?

This is the day-to-day use. Take a manifest before the step, another
after, and ask what moved:

``` r

before <- objManifest(g)

g <- createSpatialNetwork(g, name = "tutorial_net", method = "Delaunay")

manifestDiff(before, objManifest(g))$summary
#> [1] "spatial network added: tutorial_net"
```

Steps that overwrite something already present are the interesting case.
Recomputing an existing network leaves its name and its place untouched,
and only the content changes. The fingerprint is what sees it:

``` r

before <- objManifest(g)

g <- createNearestNetwork(g, name = "sNN.pca", k = 10)
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [aggregate][rna] sNN.pca
#> Setting nearest neighbor network [aggregate][rna] kNN.pca

manifestDiff(before, objManifest(g))$summary
#> [1] "NN network added: kNN.pca | NN network sNN.pca modified (directed, fingerprint)"
```

A step that changes the cells changes nearly everything at once. The
count change is stated once and the records that moved with it are
tallied, rather than listed one by one:

``` r

gs <- subsetGiotto(g, cell_ids = head(spatIDs(g), 100))
manifestDiff(objManifest(g), objManifest(gs))$summary
#> [1] "cells [z0]: 498 -> 100 | cells [z1]: 504 -> 100 | cells [aggregate]: 462 -> 100 | 27 objects changed"
```

[`manifestDiff()`](https://giotto-suite.github.io/GiottoClass/dev/reference/manifestDiff.md)
also returns `changed` (a flag) and `detail` (every affected record, as
data). The one-sentence `summary` is what you would hand to a person, or
a model; `detail` is what you would score or assert against.

## Fingerprints

Every record carries a fingerprint: a short hash that changes when the
content changes. Cell metadata goes one step further and fingerprints
each column, so a diff names the column an edit touched, even an edit to
a single cell that leaves the column’s levels unchanged:

``` r

before <- objManifest(g)

cm <- getCellMetadata(g, output = "cellMetaObj")
lv <- unique(cm[]$leiden_clus)
cm[]$leiden_clus[1] <- setdiff(lv, cm[]$leiden_clus[1])[[1]]
g_edit <- setGiotto(g, cm, verbose = FALSE)

manifestDiff(before, objManifest(g_edit))$summary
#> [1] "cell metadata changed: leiden_clus"
```

Level counts are not reported by default: counting the distinct values
of every column is a full pass over the table, and the fingerprints
already say which column changed. Ask for them with `levels = TRUE` and
the diff says what a re-clustering produced:

``` r

cm2 <- getCellMetadata(g, output = "cellMetaObj")
cm2[]$leiden_clus <- cm2[]$leiden_clus %% 3
g_regroup <- setGiotto(g, cm2, verbose = FALSE)

manifestDiff(objManifest(g, levels = TRUE),
             objManifest(g_regroup, levels = TRUE))$summary
#> [1] "cell metadata changed: leiden_clus (5 -> 3 levels)"
```

How a fingerprint is computed depends on what the subobject holds:

| Payload | Fingerprinted from |
|----|----|
| matrices, tables, graphs, view and space steps | all of their values, exactly |
| polygons and points | a fixed-stride sample of the geometries and their attributes, plus the overall extent |
| images | the pixels read at display resolution, plus extent |
| disk-backed stores | the store’s handle: its identity, recorded operations and schema |
| BPCells and HDF5Array matrices | their delayed operations, and the names and sizes of their backing files |
| functions | their code; values a closure captures are not seen |

A fingerprint is never a hash of the R object itself. Objects that hold
an external pointer or an environment hash differently every time they
are created, even when their content is identical; a fingerprint is
built from the content extracted out of them. It is deterministic and
never touches the RNG.

It costs little. In-memory content of half a million cells hashes in a
few tens of milliseconds, and an image costs the same whatever its size.

Two comparisons are deliberately loose. Geometry coordinates are
compared as a set of vertices to 12 significant digits, because writing
a polygon to a shapefile and reading it back reorders its vertices and
drops the last bits of a double. And images are read at display
resolution, which uses a file’s overviews when it has them; so the same
pixels saved in a format with overviews, such as a COG, fingerprint
differently once.

Payloads GiottoClass does not know get their fingerprint from the
package that provides them, through the
[`fingerprint_data()`](https://giotto-suite.github.io/GiottoClass/dev/reference/fingerprint_data.md)
generic. A payload with no method has no fingerprint, and the manifest
says so in `warnings`. The diff says so too, instead of claiming nothing
changed:

``` r

gu <- g
e <- gu@expression$aggregate$rna$raw
e@exprMat <- structure(list(), class = "someNewFormat")
gu@expression$aggregate$rna$raw <- e

mu <- objManifest(gu)
mu$warnings
#> [1] "expression.aggregate.rna.raw.fingerprint"
manifestDiff(mu, objManifest(gu))$summary
#> [1] "no detected change (1 item unfingerprinted)"
```

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
#>  $ timestamp: chr "2026-10-22T20:29:24Z"
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
and a direct slot assignment calls nothing at all, so an object’s
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
#> [1] "cell metadata added: hand_label"

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
without loading it:

``` r

sc <- jsonlite::fromJSON(file.path(saved, "manifest.json"),
                         simplifyVector = FALSE)

unlist(sc$summary$spat_units)
#> [1] "aggregate" "z0"        "z1"
unlist(sc$summary$n_cells)
#>        z0        z1 aggregate 
#>       100       100       100
vapply(sc$slots$expression, function(r) {
    paste(r$keys$spat_unit, r$keys$name, sep = "/")
}, character(1))
#> [1] "aggregate/normalized" "aggregate/pearson"    "aggregate/raw"       
#> [4] "aggregate/scaled"     "z0/raw"               "z1/raw"
```

Fingerprints survive the round trip: an object saved and loaded back has
the same fingerprints as the one that was saved, so a diff across a save
reports only what actually changed.

[`objManifest_json()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest_json.md)
and
[`objHistory_ndjson()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objHistory_ndjson.md)
produce the same two documents for any object, whether or not you are
saving it. The JSON follows a versioned schema that ships with the
package:

``` r

system.file("schema", "giotto-manifest-0.2.0.json", package = "GiottoClass")
#> [1] "/home/runner/work/_temp/Library/GiottoClass/schema/giotto-manifest-0.2.0.json"
```

Records are sorted by their keys so two manifests of the same state
serialize identically, and `NA`, `NaN` and `Inf` are encoded as strings
so that a missing value stays distinguishable from a not-a-number.

## What it is good for

- Checking that a step did what you expected, and noticing when it did
  nothing.
- Inspecting a saved object without loading it.
- Regression-testing a pipeline: same inputs should give the same
  fingerprints.
- Diffing two runs, or the same analysis before and after a parameter
  change.
- Turning `history.ndjson` into a methods section.
- Giving an assistant, an eval harness or a reviewer machine-readable
  state instead of printed text.

## What it does not do

- **Sizes are not reported.** Dimensions, row counts and extents would
  each cost a scan on a disk-backed object, so records carry none; the
  summary block’s cell and feature counts are the exception.
- **Sampled payloads can miss a local edit.** Moving one polygon that
  falls outside the geometry sample, or painting a few pixels between
  the points an image is read at, can leave the fingerprint unchanged.
  Matrices, tables and graphs are hashed in full and have no such gap.
- **Disk-backed payloads need a method.** A store is fingerprinted only
  once the package that provides it implements
  [`fingerprint_data()`](https://giotto-suite.github.io/GiottoClass/dev/reference/fingerprint_data.md).
  Until then it is listed in `warnings` and the diff counts it as
  unfingerprinted.
- **The history is not complete by construction.** It records what
  functions chose to log. Entries written with `toplevel = 1` name
  `update_giotto_params` rather than the calling function, a property of
  those call sites, not of the record.
- **The schema is 0.2.0 and not yet released.** Treat the field set as
  stable in spirit, not frozen.
