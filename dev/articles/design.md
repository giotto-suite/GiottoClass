# Architecture and Design

This article describes the internal architecture of GiottoClass for
contributors and advanced users. It covers the two-tier object model,
the schema that keys data inside it, the subobject class hierarchy, and
the reasoning behind the decisions that constrain how new code attaches.

It is the *why* companion to `AGENTS.md`, which is the terse
present-tense statement of what holds now and where the code lives.
`vignettes/overview.Rmd` is the user-facing walkthrough of the same
object model. Where a decision was contested or has a cost worth
remembering, it has an `adr/` record; those are cited inline below
rather than restated.

Two subsystems are large enough to have their own articles, summarised
here and recorded in full there: `design_gmulti.Rmd` for multi-sample
federation, and `design_view_space.Rmd` for the view / space recipe
layers.

## What GiottoClass is for

GiottoClass owns the object model for the Giotto suite and nothing else.
It defines the `giotto` container, its subobjects, the accessors, the
spatial manipulation verbs, and the generics that analysis packages
attach methods to. It deliberately ships **no analysis methods** — no
normalization, no PCA, no clustering. The split is what lets a
representation (in-memory matrix, a GiottoDisk `parquetExprStore`, a
BPCells `IterableMatrix`) and an algorithm meet without either package
depending on the other.

Two consequences shape most of the code:

- Every downstream suite package depends on this one, so a change to a
  class definition or an accessor contract is a suite-wide event.
  Backwards compatibility is handled explicitly (see *Object versioning*
  below) rather than by keeping definitions frozen.
- The container has to hold data it cannot inspect cheaply. A slot may
  contain a `dgCMatrix` in memory or a handle to a hundred-gigabyte
  parquet dataset. Anything that “just checks” the contents of a slot
  has to be assumed expensive, and much of the design is arranged so
  that it does not have to.

## The two-tier object model

There are exactly two tiers, and the boundary between them is
load-bearing.

**Tier 1 — the `giotto` container.** 24 slots, most of them
`nullOrList`. The container holds no data of its own; each data slot is
a nested list of tier-2 subobjects. It owns identity (`@cell_ID`,
`@feat_ID`), session configuration (`@instructions`), provenance of the
analysis run (`@parameters`), versioning (`@versions`), and the storage
backend (`@source`).

**Tier 2 — the subobjects.** `exprObj`, `spatLocsObj`, `cellMetaObj`,
`featMetaObj`, `giottoPolygon`, `giottoPoints`, `dimObj`, `nnNetObj`,
`spatialNetworkObj`, `spatialGridObj`, `spatEnrObj`, `giottoImage` and
friends. Each wraps one piece of data plus the schema tags that say
where it belongs.

A subobject is a complete, self-describing unit. It can be pulled out,
operated on, and put back — and every spatial verb (`spin`, `flip`,
`rescale`, `crop`, `affine`, `spatShift`, `ext`) is defined on
subobjects first and on the container second, where the container method
is largely a dispatch-and-reassemble loop. This is why
[`getSpatialLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialLocations.md)
returns a `spatLocsObj` rather than a `data.table` by default, and why
`x[]` is the idiom for reaching the payload inside: keeping the wrapper
means keeping the tags, and dropping to a bare `data.table` throws away
the object’s knowledge of where it came from.

## The schema

Data inside the container is addressed by a small fixed vocabulary
rather than by position. Four components, each with a getter/setter pair
([`?giotto_schema`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)):

| component | generic | what it keys |
|----|----|----|
| spatial unit | [`spatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md) | the length scale or polygonal annotation being used as the unit of study — `"cell"`, `"nucleus"`, a binned grid |
| feature type | [`featType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md) | the data modality — `"rna"`, `"protein"`, … |
| name | [`objName()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md) | which object of that kind — `"raw"`, `"normalized"`, `"pearson"` |
| provenance | [`prov()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md) | which spatial units the data was *derived from* |

The first three are the nesting keys, and **the nesting depth of a slot
is exactly the set of keys that slot needs** — there are no placeholder
levels:

``` r

gobject@expression$cell$rna$raw     # exprObj      — spat_unit / feat_type / name
gobject@spatial_locs$cell$raw       # spatLocsObj  — spat_unit / name (no modality)
gobject@cell_metadata$cell$rna      # cellMetaObj  — spat_unit / feat_type (one per key)
```

So the depth-varying access in `R/slot_accessors.R` is not incidental
complexity; it is the price of not storing levels that carry no
information.

Provenance is the fourth component and does not participate in nesting.
It exists because a spatial unit can be *composed* from others —
aggregating transcripts into cells, or binning cells into a grid — and
the resulting object is legitimately keyed under the new unit while
needing to record the units it was built from.
[`.prov_match()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-prov_match.md)
is what checks that a set of subobjects being combined actually share an
origin.

### Why nested lists rather than one indexed table

A flat registry (one table, one row per artifact, keyed by
`(spat_unit, feat_type, name)`) would be more uniform and would make
listing cheaper. It was not used, and the reason is that the nesting
*is* the index: `names(gobject@expression)` answers “which spatial units
have expression data” without touching any of the data, and a subset by
spatial unit is a list subset rather than a filter-and-rebuild. The
uniformity cost is real and shows up as the depth-varying access helpers
in `R/slot_accessors.R`.

## Subobject class hierarchy

Every subobject inherits from several virtual classes at once, and they
come from two independent families. Reading a `contains =` line means
splitting it into those two families first.

**Family 1 — which schema tags the object carries.** These are what the
accessors in the previous section dispatch on.

    provData      # has @provenance
    featData      # has @feat_type
    spatData      # has @spat_unit          (contains provData)
    spatFeatData  # both axes               (contains spatData, featData, provData)
    nameData      # has @name

**Family 2 — how the payload is stored.** These are what the data
operators dispatch on.

    gdtData (VIRTUAL)   # data.table-backed; behaves like a data.table under [, $, nrow
    ├── coordDataDT     → spatLocsObj
    └── metaData        → cellMetaObj, featMetaObj
    exprData            → exprObj                     (a matrix, or a store handle)
    terraVectData       → giottoPolygon, giottoPoints  (an external pointer)

A third, smaller set names the domain where it earns its place —
`enrData`, `nnData`, `spatNetData`, `spatGridData`, `miscData`. So
`spatEnrObj` is simultaneously `nameData`, `enrData`, `spatFeatData` and
`gdtData`, and each of those four contributes a different set of
inherited methods.

The reason for the split is that the two families cross-cut.
`cellMetaObj` and `spatLocsObj` are both `data.table`-backed and share
`[`, `$`, `nrow` and `colnames` through `gdtData`; but `spatLocsObj`
carries only a spatial unit while `cellMetaObj` carries both axes.
Meanwhile `exprObj` and `giottoPolygon` are both `spatFeatData`-adjacent
but have nothing in common at the storage layer — one is a matrix or a
lazy store handle, the other an external pointer to C++ memory. A single
hierarchy grouped by domain would have forced the shared `data.table`
behaviour to be reimplemented per domain; one grouped by storage would
have done the same to the schema accessors.

The practical rule for a new subobject: pick one virtual from each
family, then add a domain virtual only if there is behaviour specific to
that domain to hang on it.

### terra-backed classes and the pointer problem

`giottoPolygon` and `giottoPoints` wrap
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html),
which is an external pointer to C++ memory. Pointers do not survive
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html), and they do not
survive being sent to a parallel worker.

There are **two separate answers**, and which one applies depends on
where the object is going.

**Serialization by value —
[`wrap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
/
[`vect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
and the `packed*` classes.**
[`terra::wrap()`](https://rspatial.github.io/terra/reference/wrap.html)
copies the geometry out of C++ memory into an R-side representation.
`packedGiotto`, `packedGiottoPolygon` and `packedGiottoPoints` hold
those payloads, and
[`wrap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)/[`vect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
convert in both directions.

**This pattern is unmaintained and may be removed. Do not build on it**
(ADR 0007). Copying every layer into the R heap does not hold at project
scale, and the by-reference path below covers persistence while a
`gsource` backend covers reaching data from a worker process. It stays
in place only because
[GiottoData](https://giotto-suite.github.io/GiottoData/)’s
`giottoPoints` / `giottoPolygon` minis and older user `.RDS` files are
stored as `packedGiotto*` and still have to read back.

One property worth understanding before it goes: `packedGiotto`
deliberately **does not inherit from `giotto`**. It carries the same
slot names but no method inheritance, so a packed object cannot silently
satisfy a method expecting live pointers — the failure is a dispatch
error rather than a silently empty result. Whatever replaces it should
preserve that.

**Serialization by reference —
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
/
[`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md).**
This is *not* built on
[`wrap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md).
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
writes a directory, and each `SpatVector` goes out as its own shapefile
via `.save_external()` →
[`terra::writeVector()`](https://rspatial.github.io/terra/reference/writeVector.html)
(plus a `_names.txt` sidecar, because shapefiles truncate column names).
Only then is the gobject itself `saveRDS`’d, still holding its
now-meaningless pointers.
[`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
reads the RDS back and `.load_external()` re-reads each shapefile into
the corresponding slot.

Why the by-reference path won: writing shapefiles keeps peak memory at
one layer and leaves artifacts other tools can open, where
[`wrap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/wrap.md)
produces one opaque blob and needs the whole project resident to build
it. The cost accepted in exchange is that a saved Giotto object is a
*directory*, not a file, and moving it means moving all of it.

**The failure mode to recognize.** A `SpatVector` whose pointer did not
survive does not error on access — it behaves as an empty or invalid
object. So a bare `saveRDS(gobject)` “works”, and the damage only shows
up later as zero features or a dropped extent. That silence is why
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
exists and why reaching for
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) on a gobject is never
right.

[`reconnect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
covers the raster half. A `SpatRaster` is not wrapped at all: it holds a
live handle to a file on disk,
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
writes it out with
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
and records the new path on the image object, and
[`reconnect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
re-opens from that path on load.

## Federation — many samples in one object

There is a third container, and it is a sibling of `giotto` rather than
a subclass of it:

    gAny (virtual)
    ├── giotto
    └── giottoMulti

`giottoMulti` presents N child `giotto` objects as one analysable unit.
It does **not** inherit from `giotto`: its per-dataset spatial slots are
absent, not inherited-empty, and inheriting would let a spatial-domain
method fall through to an empty slot and return a plausible-looking
nothing. With a virtual base, an undefined method fails loudly through
no-method dispatch, while shared-domain methods are written once.

The organising rule is a split by what can be decomposed. **Non-spatial
content is joint** — expression, metadata, dimension reductions,
enrichments live in slots at the parent, assembled across children.
**Spatial content is not**, and stays on the children, because a
location, a polygon, a point or a raster each belongs to exactly one
sample and a per-sample representation of it always exists. A
cross-sample network edge is the one artifact that fails that test — its
endpoints are in two samples — so `@spatial_network` is the one spatial
slot at the parent.

Three declaration layers keep the federation addressable, and they are
routinely confused with one another:

| layer | declares | scope |
|----|----|----|
| `@mapping` | which child-level handles a parent-level handle federates | the indexing axes — spat_unit, feat_type, values |
| `@groups` | that a name refers to several samples | sample membership, by enumeration |
| `@spaces` | a coordinate frame | frame only |

`@mapping` is what lets children spell the same modality differently —
`"rna"` in one sample, `"transcripts"` in another — without renaming
data on disk. It is auto-discovered at construction and editable
afterwards.

Two properties are load-bearing and easy to collapse into one another.
`@id_map` is the **identity registry**, a global vocabulary of which IDs
exist in which child, and it is *never* narrowed. `@cell_ID` /
`@feat_ID` are the **active narrowing**, and `NULL` means unfiltered.
Keeping them apart is what lets a narrowing be widened or dropped
without having lost the population. Children are never mutated by any of
this; narrowing lives at the parent only.

The joint slots are simultaneously a cache and the source of truth: the
first access to a federated handle assembles it from the children, and
everything derived at parent scope lives there natively. The trap is
that they are *lazily populated* — empty on a fresh multi, and
[`getExpression()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getExpression.md)
does not cache back — so any logic deriving keys from
`names(@expression)` silently sees nothing.

`vignettes/articles/design_gmulti.Rmd` is the full record: the access
layer, the carry-keys discipline that alignment between independently
assembled joint slots requires, what was tried and removed, and what is
still open.

## `initialize()` as the consistency engine

The `giotto` class has an unusually heavy `initialize()` method, and it
runs on every set operation, on instruction updates, and on load. Its
job is to make “the object is internally consistent” a property that
holds continuously rather than something checked at the end.

In order, it updates the object and subobjects for class definition
changes, ensures instructions exist, checks the python environment when
conda options are on, validates the active `spat_unit`/`feat_type`,
initializes `@cell_ID` / `@feat_ID` and the metadata tables for the
active keys, then runs hierarchical checks that dependent data is only
present after the data it depends on.

The important design property is the **ID derivation preference**: IDs
are pulled from `@spatial_info`/`@feat_info` or from `@expression`, with
expression preferred. That ordering is what makes it legal to build an
object out of order — set polygons first or expression first — and still
converge on one answer for what the cells are.

The cost is that `initialize()` is on the hot path of every setter. Code
that sets many objects in a loop should prefer the `initialize = FALSE`
argument where a setter exposes it and re-initialize once at the end.

## Object versioning

`@versions` records the GiottoClass version that produced the object,
and
[`updateGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoObject.md)
is a linear sequence of version-gated migrations.

Two mechanisms make migration possible without freezing class
definitions:

- **[`attr()`](https://rdrr.io/r/base/attr.html) reads for removed
  slots.** R’s S4 deserialization keeps the stored representation when a
  class definition has changed, so a removed slot is still reachable via
  `attr(gobject, "old_slot")`. This is how `@OS_platform`,
  `@largeImages` and the legacy network slots were retired without
  breaking old files — see `.update_network_slots()` (ADR 0004) for the
  pattern applied to a slot rename plus a content conversion.
- **Version-gated blocks.** Migrations are guarded on
  `.gversion(gobject) < numeric_version("x.y.z")` so they are skipped
  for objects that never had the problem.

An object created by a *newer* GiottoClass than the loaded one warns
rather than fails — forward compatibility is best-effort.

## Accessors

Three surfaces, deliberately distinct:

- **`get*()` / `set*()`** — the typed, slot-specific pairs in
  `R/slot_accessors.R`. These take `spat_unit` / `feat_type` / `name`
  explicitly and are what user code and analysis packages call. `set*()`
  is where write-through to a storage backend happens (ADR 0002).
- **`setGiotto(gobject, x)`** — the untyped setter. It reads the schema
  tags off the subobject itself and routes to the right slot, so a
  subobject that has been pulled out, transformed, and is being put back
  does not need its destination restated. This is the generic that
  spatial verbs on the container use internally.
- **`[` / `[[`** — the terse interactive surface, and `x[]` on a
  subobject to reach the payload.

The asymmetry to keep in mind: `get*()` returning a subobject is the
default because the tags travel with it; several getters take `output =`
to drop to a bare `data.table` or matrix, and that is a one-way door.

## Views and spaces — deferred narrowing and reframing

Two recipe layers sit over any `gAny`, so they work on a plain `giotto`
as well as on a `giottoMulti`. They change what subsequent accessors see
without mutating data.

- **`giottoView`** — read-only narrowing: which cells and features are
  in scope.
- **`giottoSpace`** — a coordinate frame: where the data sits. Not
  read-only; running an analysis in a non-native frame is fine, the
  coordinates just differ, and mutations still target the underlying
  data in its native frame.

They are two classes rather than one because a single recipe carrying
both could not answer “in what frame?” for a crop extent, could not tell
an artifact generator whether its transforms or its filters were the
meaningful part, and made “view” mean both selection and positioning in
the same vocabulary.

**There is no constructor; a recipe is created by recording.** The first
[`subset()`](https://rdrr.io/r/base/subset.html) /
[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
/
[`selectSamples()`](https://giotto-suite.github.io/GiottoClass/dev/reference/selectSamples.md)
call naming a view creates it, and a transform verb naming a space does
the same; later calls with that name append. Eager behaviour is
unchanged — `spin(g, 30)` still spins the object — and the presence of
`space =` is what switches from acting to recording. Consequently
`view =` and `space =` on a public function take a **name**, never a
handle: a detached recipe on a generator writes a frame name that
resolves against nothing, and on a reader returns content in a frame the
object cannot name (adr/0006).

**The steps are plain tagged lists; the containers are classes.** That
division is where the serialization guarantee lives — a step carries no
closure and no external pointer, so a predicate is stored deparsed, a
crop region as WKT, and transform arguments whitelisted — while the
container remains the handle that `[`, the builder verbs and
[`as.list()`](https://rdrr.io/r/base/list.html) act on.

Two invariants are worth knowing before touching any consumer:

- **A crop resolves to a cell_ID set**, and every cell-keyed target
  narrows by that same set — locations, metadata, expression and a
  backed polygon store alike. A recipe cannot mean different things
  depending on which slot it is read through. What derives the set
  (`geom = "centroid" | "poly"`) is declared on the step rather than
  inferred, so a serialized recipe states which question it asks and a
  backed resolver reads the field instead of re-deriving it.
- **Predicate frame and output frame are separate.** The frame a crop
  region was drawn in lives on the crop step; the frame results come
  back in is the explicit `space =` argument, with no fallback between
  them. That is what lets an ROI drawn on a rotated image select cells
  that are returned in native coordinates.

`materialize(g, view, space)` applies the recipes and returns a new
gobject whose ordinary accessors see the narrowed, reframed data. It is
plumbing rather than a user verb — exported so GiottoVisuals can call it
— and the user-facing route is the `view =` / `space =` arguments on the
accessors themselves.

`vignettes/view_and_space.Rmd` is the user-facing walkthrough.
`vignettes/articles/design_view_space.Rmd` is the full design record:
the space kinds and why the per-sample one is free, why the native frame
has no name, how scope is stated per call rather than inherited from
build order, parent-only evaluation on a multi, and the deferrals.

## Analysis verbs

GiottoClass defines five generics with signature `(x, param, ...)` and
exports no methods for them (ADR 0003):

| generic | return contract |
|----|----|
| [`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md) | same-shape transform — normalize, scale |
| [`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md) | a selection — IDs to keep |
| [`reduceData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceData.md) | a decomposition / embedding |
| [`clusterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/clusterData.md) | cluster assignments; `param` takes `bluster::BlusterParam` |
| [`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md) | computed outputs and summary statistics |

The split is by **return contract**, not by topic. A single
`processData(x, param)` surface with the `Param` subclass selecting the
operation would dispatch correctly and be smaller, but the return type
would then be a function of the `param` argument, and no consumer could
be written against the generic. Five generics means five stable
contracts.

Each has a matching virtual `Param` class (`processParam`,
`filterParam`, `reduceParam`, `analyzeParam`) carrying a single `param`
list slot. The Param object is where an operation’s identity and its
arguments live, which is what lets `x` dispatch on representation and
`param` dispatch on algorithm independently.

**This is the extension point.** A new algorithm is a new `Param`
subclass plus a method in Giotto or GiottoDisk. It is not a new generic
here, and adding one because an operation “feels different” is the
mistake this design exists to prevent — route to the generic whose
return contract matches, or fall through to the `ANY,ANY` catch-all.

`createNetwork(x, param)` follows the same shape for network
construction (ADR 0004): one generic dispatching on data class ×
`networkParam` subclass, with the four legacy constructors reduced to
wrappers that build a Param.

## Networks

Both network classes store a graph in a slot named `network`: an
`igraph` (undirected for spatial, directed for kNN) or a GiottoDisk
`dataStore`. `spatialNetworkObj` additionally keeps `unfiltered`.

The earlier design held a `data.table` of edges *carrying coordinate
columns*, which duplicated data already in `spatLocsObj` and needed
explicit full/reduced conversion helpers to emulate undirected semantics
a graph has natively. ADR 0004 has the full argument.

### A network is edges; everything else is attached

That decision fixes a seam worth stating directly, because it is easy to
read the getter as withholding something.
`getSpatialNetwork(output = "networkDT")` returns `from`, `to` and the
edge attributes, and that is the whole of what a network stores: the
vertices carry a name and nothing else. Anything about the *cells* an
edge runs between — a cluster label, a position — is not held back, it
is absent.

Attaching it is
[`annotateSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/annotateSpatialNetwork.md)’s
job, and its two annotations are one operation on two sources: take a
cell-keyed value and write it onto each end of the edge.
`cluster_column` resolves through
[`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md),
so a label may come from metadata, expression or enrichment alike.
`coordinates` reads the spatial locations and emits `sdim[xyz]_begin` /
`_end`. Both are optional, so a caller pays only for what it reads — a
plot asks for coordinates and no label, a proximity analysis asks for a
label and no coordinates. `.attach_edge_coords()` holds the join, and
`plot(spatialNetworkObj, spatLocsObj)` is the subobject-level entry to
it; a network alone refuses to draw, because it has no geometry to draw
with.

Three things follow, and all three are consequences of not storing
coordinates rather than rules anyone has to enforce.

**Coordinates are read live.** A transform applied to the locations is
carried by anything that joins against them, so no consumer needs
frame-aware code for networks. `distance` is the one scale-sensitive
field a network does carry, and nothing derives geometry from it.

**The join is inner, and that is what narrowing means for a network.**
An edge whose endpoint is absent from the locations has no position, so
it is dropped. Narrow the locations — through a view, a subset, a crop —
and the edges follow, with no narrowing logic anywhere in the network
path. The result is the induced subgraph, which is deliberately *not*
the network you would get by rebuilding on the subset: a Delaunay
triangulation of the survivors is not the original with vertices
deleted. Keeping this a property of the attachment rather than of the
stored object is what stops the two being confused.

**Nodes are not the network’s to supply.** An edge table has no row for
a cell with no edges, in this or any schema, so the node set comes from
the spatial locations. A cell that survives a narrowing but keeps no
neighbours is still a cell; a consumer that took its nodes from the
edges would silently lose it.

One consequence still outstanding: accessors that reach into `@network`
assuming `igraph` —
[`spatIDs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
on network objects among them — need a `dataStore` branch or a
polymorphic `nodeIDs()` generic. The gap is recorded in ADR 0004’s
consequences.

## Instructions

`giottoInstructions` is session configuration attached to the object:
python path, plot save/show/return behaviour, output directory, dpi,
format. It rides along in `@instructions` so that a function called deep
in an analysis can find the user’s plotting preferences without them
being threaded through every call.

The mechanism is a deliberate trade: it makes the object carry state
that is not data, and an object moved between machines carries a python
path that may not exist there.
[`instructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
is re-checked on `initialize()` for that reason.

## The disk-backed arc

`@source` holds a `gsource`-inheriting backend manager (GiottoDisk’s
`gDirSource`) or `NULL`. `NULL` is the one test for “is this object
backed” (ADR 0001) — there is no null-object standing in for “in
memory”, so every write-through site guards explicitly.

When an object is backed, `set*()` writes the payload to the vault and
stores the resulting `dataStore` handle in the slot rather than the
in-memory value (ADR 0002). The intent is that peak memory is one
analysis step rather than the whole project, and that there is an
addressable artifact on disk between checkpoints —
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
is the checkpoint, not the write.

Two properties follow that surprise callers:

- Setters on a backed object perform I/O. They can be slow and they can
  fail.
- A slot may hold a store rather than a matrix, so anything that assumes
  [`is.matrix()`](https://rdrr.io/r/base/matrix.html) or reaches for
  [`dim()`](https://rdrr.io/r/base/dim.html) on slot contents needs a
  store branch. Validators are the sharp case: `.evaluate_*()` functions
  pass store classes through unchanged, because a validator that
  materializes its input defeats the whole arrangement.

`@h5_file` is the deprecated predecessor — a path string, shaped around
a single HDF5 file, unable to carry the behaviour a vault manager needs.
It is kept in the class definition for one release so older serialized
objects still load.

## Interoperability

`R/interoperability.R` holds conversions to and from Seurat (v4 and v5),
`SingleCellExperiment` / `SpatialExperiment`, AnnData, and SpatialData.
These are lossy in both directions by necessity — the schema described
above has no counterpart in most of those formats, so a round trip does
not preserve provenance or multi-unit nesting.

They are the largest source of `Suggests:` dependencies, and every one
is guarded by `package_check()` rather than imported.

## Dispatch outside S4

`R/flex_functions.R` holds `*_flex` helpers (`mean_flex`,
`colSums_flex`, …). These exist because Giotto needs to route to a
method that is *not* the one defined for a class in its native package —
most often to keep sparse `Matrix` operations from silently densifying —
and doing that by defining S4 methods would pollute the user’s
environment with dispatch they did not ask for.

Code inside the suite should use the flex function as a drop-in
replacement for its generic counterpart. Adding a new one is preferable
to a bare `setMethod` on a base generic for a class the package does not
own.

## Where things live

| directory | contents |
|----|----|
| `R/classes*.R` | class definitions, one file per family |
| `R/generics.R` | every `setGeneric` (43 of them), no implementations |
| `R/methods-<topic>.R` | method implementations, one file per operation |
| `R/slot_accessors.R` | the typed `get*`/`set*` pairs — the largest file in the package |
| `R/package_imports.R` | all `@importFrom` declarations, centralized |
| `adr/` | decision records — why, and what was rejected |
| `vignettes/` | user-facing walkthroughs |
| `vignettes/articles/` | contributor-facing design docs (this file, `design_gmulti.Rmd`, `design_view_space.Rmd`); not built as vignettes |

The generic/method file split is not cosmetic: a generic defined next to
one of its methods reads as belonging to that method, and the next
person adds a second generic instead of a second method.
