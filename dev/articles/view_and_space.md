# giottoView and giottoSpace: recipes for filtering and reframing

## What they are

`giottoView` and `giottoSpace` are **non-destructive recipes** attached
to a `giotto` / `giottoMulti` object that change what subsequent
accessors see without mutating the underlying data.

|  | `giottoView` | `giottoSpace` |
|----|----|----|
| Records | filter / crop / sample-selection steps | coordinate-frame transforms (spin, affine, flip, …) |
| Effect on accessors | narrows which cells survive | reframes spatial geometry of survivors |
| Use case | “show me cluster 3 inside this region” | “rotate sample B by 45° and shift to land next to sample A” |
| Built by | [`subset()`](https://rdrr.io/r/base/subset.html) / [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md) with `view = "name"` | a transform verb with `space = "name"` |
| Stored as | a `giottoView` under `gobject@view$name` | a `giottoSpace` under `gobject@spaces$name` |

Both are created by *recording* onto a name: there is no constructor and
no standalone builder. The first call naming a view or space creates it;
later calls with the same name append.

Each is an S4 object holding an ordered list of **steps**, and each step
is a plain tagged list carrying no closure and no external pointer. That
is what lets a recipe survive
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and reach a parallel
worker — a filter predicate is stored deparsed, a crop region as WKT.

They’re separate by design: a view is a cell-identity filter (predicates
on metadata or expression, optionally a spatial crop bbox), while a
space is a coordinate transform stack. The same view can be applied in
any frame; the same space can be applied to any cell subset.

## When to use which

- **Filter cells:** `giottoView`. Predicates over metadata or
  expression, optional crop region, optional sample selector.
- **Reposition cells:** `giottoSpace`. Spin / affine / shift / flip /
  rescale composed in order. Sample-keyed so different children of a
  `giottoMulti` can carry different transforms.
- **Both:** apply the view first to pick which cells, then the space to
  decide where to draw them. The `view=` / `space=` args on getters
  accept both.

## Recording a view

[`subset()`](https://rdrr.io/r/base/subset.html) gains a `view =`
argument. With it, the call records a filter step under that name
instead of narrowing the object, and returns the gobject:

``` r

library(GiottoClass)

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
#> > " Delaunay_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] Delaunay_network
#> > " spatial_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] spatial_network
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] sNN.pca
#> > 'custom_NN' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] custom_NN
#> Warning: Some of Giotto's expected python module(s) were not found:
#> pandas, igraph, leidenalg, community, networkx, sklearn
#> (This is fine if python-based functions are not needed)
#> 
#> ** Python path used: "/usr/bin/python3"

g <- subset(g, leiden_clus == 1, view = "cluster1")
giottoViews(g)
#> [1] "cluster1"
giottoView(g, "cluster1")
#> An object of class giottoView
#> steps : 1 
#>   [1] filter  leiden_clus == 1
```

The predicate is captured unevaluated and resolved later against
[`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md).
Free variables in it (literal scalars, vectors) are substituted in at
record time, so the recipe is self-contained and does not change if the
binding does:

``` r

clusters_of_interest <- c(1, 3)

g <- subset(g, leiden_clus %in% clusters_of_interest, view = "multi")
giottoView(g, "multi")[[1]]$predicate
#> [1] "leiden_clus %in% c(1, 3)"
```

Naming the same view again appends — each step intersects with the
previous survivors:

``` r

g <- subset(g, total_expr > 500, view = "multi")
length(giottoView(g, "multi"))
#> [1] 2
```

`negate = TRUE` is folded into the recorded predicate rather than kept
as a separate field, so the step always reads as the question it will
ask:

``` r

g <- subset(g, leiden_clus == 1, negate = TRUE, view = "not_c1")
giottoView(g, "not_c1")[[1]]$predicate
#> [1] "!leiden_clus == 1"
```

### Cropping spatial region

[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
records a spatial extent (anything
[`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)
can interpret — SpatExtent, bbox, sf bbox, numeric vector) plus an
optional `relation` and `geom`:

``` r

g <- crop(g, terra::ext(6500, 7000, -4500, -4000), view = "box")
giottoView(g, "box")[[1]]$relation
#> [1] "intersects"
```

`relation` is the spatial predicate; `geom` says what represents a cell
when that predicate runs. Both are recorded on the step — see [What a
crop predicate is evaluated
against](#what-a-crop-predicate-is-evaluated-against) for which
relations are available and how the two interact.

[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
is not limited to rectangular extents — any geometry that
[`terra::is.related()`](https://rspatial.github.io/terra/reference/relate.html)
can evaluate against is accepted. Pass a `SpatVector` polygon (or an
`sf` polygon, WKT, etc.) directly as `y`, and the same `relation` and
`geom` arguments apply:

``` r

# Build an arbitrary ROI polygon (or pull one from gobject@spatial_info,
# a hand-drawn annotation, an imported GeoJSON, ...)
roi <- terra::vect(
    matrix(c(6500, -4500,
             7100, -4400,
             6900, -4000,
             6500, -4200,
             6500, -4500), ncol = 2, byrow = TRUE),
    type = "polygons"
)

# cells whose centroid is inside roi
g <- crop(g, roi, relation = "within", view = "roi")
```

Bbox crops take an AABB short-circuit at resolution time;
non-rectangular regions fall through to a
[`spatRelate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatRelate.md)
call. Geometry of surviving subobjects is not modified — only the cell
set narrows.

Views mix step kinds freely, since every verb records onto the same
name:

``` r

g <- crop(g, roi, relation = "within", geom = "poly", view = "stripe1")
g <- subset(g, total_expr > 1, view = "stripe1")  # appends to the same view
```

Note that eager
[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
on a `giotto` / `giottoMulti` is **not** implemented — it would require
coordinated narrowing of every spatial subobject, images, expression,
and metadata. The `view =` path is mandatory for now; an eager call
errors with a pointer to it.

### Predicate frame vs output frame

A view’s
[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
can carry an additional `space =` arg naming the **frame the crop region
was drawn in**. It is recorded on that crop step, beside the region it
describes, so a view may crop in one frame and then another. This is a
separate concern from any output frame the consumer might request:

``` r

# A frame to draw the region in (see "Recording a space" below)
g <- spin(g, angle = 45, space = "rotate")

# Crop region drawn in "rotate" coords; the frame is recorded on the step.
g <- crop(g, terra::ext(0, 1000, -6500, -5500), view = "test", space = "rotate")

# Default: predicate uses "rotate" frame to decide cells, output stays
# in NATIVE frame.
getSpatialLocations(g, view = "test")            # native coords, narrowed
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 60 3 
#> preview   :
#>    sdimx sdimy            cell_ID
#>    <int> <int>             <char>
#> 1:  4445 -3526 AAGTAGAAGACCGGGT-1
#> 2:  4445 -4005 AATGACTGTCAGCCGG-1
#> 3:  5064 -4125 ACACAAAGACGGGTGG-1
#> 
#> ranges:
#>      sdimx sdimy
#> [1,]  3964 -4484
#> [2,]  5202 -3287

# Explicit: predicate uses "rotate" frame, output also in "rotate" frame.
getSpatialLocations(g, view = "test", space = "rotate")  # rotated coords
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 60 3 
#> preview   :
#>       sdimx     sdimy            cell_ID
#>       <num>     <num>             <char>
#> 1: 649.8311 -5636.348 AAGTAGAAGACCGGGT-1
#> 2: 311.1270 -5975.052 AATGACTGTCAGCCGG-1
#> 3: 663.9733 -6497.604 ACACAAAGACGGGTGG-1
#> 
#> ranges:
#>           sdimx     sdimy
#> [1,]   8.485281 -6497.604
#> [2,] 977.221572 -5503.412
```

The two arguments are independent:

|  | source | what it affects |
|----|----|----|
| **Predicate frame** | the crop step’s `space` (set by `crop(..., space = "name")`) | how the recorded crop region is interpreted |
| **Output frame** | explicit `space =` arg on the getter | what frame the returned coordinates are in |

The step’s `space` is read when the crop resolves and used to project
the region into whatever the output frame turns out to be — getters that
don’t pass `space =` keep returning native-frame coords. The behavior is
identical across `getSpatialLocations` / `getCellMetadata` /
`getPolygonInfo` / `getExpression`: same recipe, same cell set.

Note the arg overload: `space =` on
[`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
names the frame the region was drawn in; `space =` on transform verbs
([`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md),
[`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md),
…) records the transform as a step onto the named space recipe. Same
referent (the slotted space `"name"`), different operation per verb.

### Sample selection (giottoMulti only)

When the view will be applied to a `giottoMulti`, `subset(samples = )`
records which children participate. The step is resolved first, before
any filter or crop steps, and the excluded children are never read. One
call can record both a sample step and a filter:

``` r

mg <- createGiottoMulti(list(
    visium_a = GiottoData::loadGiottoMini("visium", verbose = FALSE),
    visium_b = GiottoData::loadGiottoMini("visium", verbose = FALSE)
))
#> > " Delaunay_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] Delaunay_network
#> > " spatial_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] spatial_network
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] sNN.pca
#> > 'custom_NN' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] custom_NN
#> > " Delaunay_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] Delaunay_network
#> > " spatial_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] spatial_network
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] sNN.pca
#> > 'custom_NN' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] custom_NN
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

mg <- subset(mg, leiden_clus == 1, samples = "visium_a", view = "pair")
names(getSpatialLocations(mg, view = "pair"))
#> Getting values from [cell][rna] cell metadata
#> [1] "visium_a"
```

`samples =` takes group names as well as child names (see
[`?gmultiGroup`](https://giotto-suite.github.io/GiottoClass/dev/reference/gmultiGroup.md)).
Without `view =`, `subset(mg, samples = )` slices the multi immediately,
like `mg[samples]`.

## Recording a space

The spatial-transform generics (`spin`, `spatShift`, `affine`, `flip`,
`rescale`) take `space = "name"` and record the call instead of
executing it. A space holds an ordered list of steps:

``` r

g <- spin(g, angle = 45, space = "rotated")
g <- spatShift(g, dx = 100, dy = 0, space = "rotated")
giottoSpace(g, "rotated")
#> An object of class perSampleSpace
#> space 'rotated' | each sample in its own copy
#>   steps   : spin -> spatShift
```

Steps apply in recorded order at resolution time. Calling a transform
without `space =` still does what it always did — transform the object
eagerly.

A space is one of two kinds, and the difference is whether its samples
**interact**. A `combinedSpace` lays them out in one coordinate system,
so a job over it is one job and cross-sample distances mean something. A
`perSampleSpace` gives each sample its own copy of the frame, so a job
over it is N independent jobs.

Recording onto an unused name always produces a `perSampleSpace`. A
`combinedSpace` has to be declared, because it has to name its members:

``` r

giottoSpace(mg, "atlas") <- combinedSpace(c("visium_a", "visium_b"))
```

Scoping a transform with `samples =` does *not* decide the kind — it
says which samples move, not whether they interact.

On a plain `giotto` there is one sample and nothing to lay out beside
it, so the distinction only bites on a `giottoMulti`.

The native frame — where the data already is — has no name.
`space = NULL` is it, and there is no second spelling: a transform
cannot be recorded onto the native frame, because the result would not
be native.

### Scoping to samples on a `giottoMulti`

On a `giottoMulti`, `samples =` says which children a transform applies
to. This is how a cross-sample layout is built — one call per child,
against the same space name:

``` r

# leave visium_a where it is; move visium_b next to it
mg <- spin(mg, angle = 90, space = "layout", samples = "visium_b")
mg <- spatShift(mg, dx = 8000, dy = 0, space = "layout",
    samples = "visium_b")
```

`samples =` accepts several children at once, and omitting it means
“every sample this space already keys” — the whole-layout case:

``` r

mg <- spin(mg, 30, space = "layout", samples = c("visium_a", "visium_b"))
mg <- spatShift(mg, dx = 10, space = "layout")   # applies to both
```

A mistyped child name is rejected at record time rather than quietly
creating a chain nothing resolves against.

`samples =` exists only on `giottoMulti` methods. A plain `giotto` has
one sample, so there is nothing to scope to, and a gmulti transform
*requires* `space =` — eager per-child transforms are not implemented,
and the cross-sample layout that motivates them is exactly what a
recorded space expresses instead.

Earlier versions built spaces standalone and merged them with `+`. That
is gone: `+` merged key sets, so a transform recorded afterwards landed
on whatever had been merged so far, and the same expression meant
different things depending on build order. `samples =` states the scope
at each call instead.

## What a crop predicate is evaluated against

A crop narrows the **cell set**, so each cell must be reduced to a
geometry before the predicate can run. `geom` declares which one:

- `geom = "centroid"` (default) — the cell’s `spatial_locs` row. Cheap,
  and the conventional choice in spatial omics;
  [`combineCellData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineCellData.md)
  documents the same. Approximate: a cell whose polygon straddles the
  region boundary with its centroid outside is dropped.
- `geom = "poly"` — the cell’s actual polygon. Exact, and needs a
  polygon source on the object.

Declaring it rather than inferring it from the relation matters because
the two genuinely answer different questions, and a saved recipe should
record which one it asks. Same relation, both representations, on the
visium mini with a 3000-unit box at the tissue centre (624 cells total):

| relation     | `geom = "centroid"` | `geom = "poly"` |
|--------------|---------------------|-----------------|
| `intersects` | 549                 | 549             |
| `within`     | 549                 | **524**         |

The `within` row is the interesting one: 25 spots have their centroid
inside the box but are not *wholly* inside it. Whether you want those is
a question about your analysis, not something a relation name can
settle. (`within` on a centroid coincides with `intersects` on a
centroid here, because for a point the two differ only when it lands
exactly on the boundary.)

Not every relation works on a centroid. `contains`, `covers`, `overlaps`
and `crosses` are always `FALSE` against a point — a point cannot
contain a polygon — so requesting one promotes `geom` to `"poly"` and
says so:

``` r

g <- crop(g, roi, relation = "contains", view = "contains_demo")
#> Warning: [crop] relation 'contains' is always FALSE against a cell centroid, so
#> it needs the cell polygon; recording geom = "poly". Pass geom = "poly"
#> explicitly to silence this.
giottoView(g, "contains_demo")[[1]]$geom
#> [1] "poly"
```

`intersects`, `disjoint`, `within` and `touches` are all well defined on
a centroid and are left alone. (`covered_by` reads like a predicate but
terra does not accept it, so it is rejected.)

Declaring `geom = "poly"` on an object with no polygon source is an
error rather than a silent fallback to the centroid answer:

``` r

# no @spatial_info on this object
g_nopoly <- crop(g_nopoly, box, geom = "poly", view = "p")
getCellMetadata(g_nopoly, view = "p")
#> Error: [crop] geom = "poly" was requested (relation 'intersects'), but
#> this object has no polygon source to evaluate it on.
#> Either add polygons (`setPolygonInfo()`) or use geom = "centroid".
```

Because the choice lives on the step instead of being re-derived per
substrate, one recipe narrows identically across every cell-keyed slot
and across in-memory objects and backed stores.

## `view =` and `space =` take a name, not a recipe

Both are `character(1)` — the key the recipe lives under. Passing a
recipe inline was considered and deliberately rejected (see
`vignettes/articles/design_view_space.Rmd`): recipes are curated, named
artifacts, and accepting one inline would make the same call site
sometimes return a gobject and sometimes return a recipe.

There is no separate constructor to reach for, because recording *is*
the construction path — the first call naming a view or space creates
it. So the accessors exist to inspect, copy, and remove, not to build:

``` r

giottoView(g, "cluster1")          # read one back
#> An object of class giottoView
#> steps : 1 
#>   [1] filter  leiden_clus == 1
giottoViews(g)                     # list the names
#> [1] "cluster1"      "multi"         "not_c1"        "box"          
#> [5] "roi"           "stripe1"       "test"          "contains_demo"
```

The setter’s remaining job is moving a recipe between objects, or
dropping one. It validates what it is handed, since the recipe is a
plain list that nothing else type-checks:

``` r

g2 <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
#> > " Delaunay_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] Delaunay_network
#> > " spatial_network " already exists and will be replaced with new
#>  spatial network
#> Setting spatial network [cell] spatial_network
#> > 'sNN.pca' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] sNN.pca
#> > 'custom_NN' already exists and will be replaced with
#>  new nearest neighbor network
#> Setting nearest neighbor network [cell][rna] custom_NN

giottoView(g2, "cluster1") <- giottoView(g, "cluster1")   # copy across
giottoView(g, "cluster1") <- NULL                          # remove
```

### Why the same verbs

The intent is that users keep the verbs they already know. The optional
`view =` / `space =` arg lets the same call site choose between “do it
now” (default) and “save the recipe for later.” No new `subsetAsView()`
/ `recordSpace()` functions to learn.

## Where recipes live

A recipe lives on the gobject from the moment it is recorded — under
`@view$name` or `@spaces$name` — and travels with it through
[`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
/
[`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md).
Downstream code refers to it by string, so nothing needs rebuilding per
call:

``` r

g <- subset(g, leiden_clus == 1, view = "cluster1_only")

giottoViews(g)
#> [1] "multi"         "not_c1"        "box"           "roi"          
#> [5] "stripe1"       "test"          "contains_demo" "cluster1_only"
giottoView(g, "cluster1_only")
#> An object of class giottoView
#> steps : 1 
#>   [1] filter  leiden_clus == 1
```

Several recipes coexist on one object, which is the point: an object can
carry competing narrowings and frames at once, and each accessor call
picks one by name.

## Reading and editing a recipe

A recipe is inspectable, and the accessors are class-preserving where
that is useful, so a recipe can be narrowed and handed on:

``` r

v <- giottoView(g, "stripe1")

length(v)          # how many steps
#> [1] 2
v[[2]]             # the second step, as a plain list
#> $type
#> [1] "filter"
#> 
#> $predicate
#> [1] "total_expr > 1"
#> 
#> $scope_args
#> named list()
v[1:2]             # a giottoView of the first two steps
#> An object of class giottoView
#> steps : 2 
#>   [1] crop    within on poly
#>   [2] filter  total_expr > 1
v[2, "space"]      # one attribute of one step: the frame its region was drawn in
#> NULL

as.list(v)         # the plain nested form, which the setter reads back
#> $stripe1
#> $stripe1$steps
#> $stripe1$steps[[1]]
#> $stripe1$steps[[1]]$type
#> [1] "crop"
#> 
#> $stripe1$steps[[1]]$region
#> [1] "POLYGON ((6500 -4500, 7100 -4400, 6900 -4000, 6500 -4200, 6500 -4500))"
#> 
#> $stripe1$steps[[1]]$relation
#> [1] "within"
#> 
#> $stripe1$steps[[1]]$geom
#> [1] "poly"
#> 
#> $stripe1$steps[[1]]$space
#> [1] NA
#> 
#> 
#> $stripe1$steps[[2]]
#> $stripe1$steps[[2]]$type
#> [1] "filter"
#> 
#> $stripe1$steps[[2]]$predicate
#> [1] "total_expr > 1"
#> 
#> $stripe1$steps[[2]]$scope_args
#> named list()
```

A space indexes on one axis at a time — `sp["visium_a"]` narrows to a
sample, `sp[[1]]` takes a step. `+` merges two recipes that are already
scoped, so it concatenates step lists and cannot change whose steps they
are.

## Applying views and spaces

### Via getter `view=` / `space=`

Most data accessors take a `view` argument (and `space`, where
coordinates matter). When supplied, the returned subobject is projected
through the recipe before being handed back:

``` r

# All cells
all_meta <- getCellMetadata(g, output = "data.table")
nrow(all_meta)
#> [1] 624

# Narrowed to cluster 1 via the view slotted above
narrowed <- getCellMetadata(g, output = "data.table",
    view = "cluster1_only")
#> Getting values from [cell][rna] cell metadata
nrow(narrowed)
#> [1] 162
```

The view is named, not passed inline — see [`view =` and `space =` take
a name, not a recipe](#view---and-space---take-a-name-not-a-recipe)
above.

### Combining view and space

The view runs first to pick cell survivors, then the space transforms
the spatial geometry of those survivors:

``` r

g <- spatShift(g, dx = 100, dy = 0, space = "layout")
getSpatialLocations(g, view = "cluster1_only", space = "layout")
#> Getting values from [cell][rna] cell metadata
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 162 3 
#> preview   :
#>    sdimx sdimy            cell_ID
#>    <num> <num>             <char>
#> 1:  5577 -4125 AAAGGGATGTAGCAAG-1
#> 2:  3995 -3047 AACCCAGAGACGGAGA-1
#> 3:  4545 -3526 AAGTAGAAGACCGGGT-1
#> 
#> ranges:
#>      sdimx sdimy
#> [1,]  3169 -5202
#> [2,]  6403 -3047
```

For plain `getCellMetadata` / `spatValues` calls, `space` is accepted
for API symmetry but is a no-op on tabular subobjects — only `cell_ID`
survival is affected. To get transformed coordinates, route through a
spatial accessor like
`getSpatialLocations(g, view = "cluster1_only", space = "layout")`.

## giottoMulti context

On a `giottoMulti`, views and spaces carry sample scope:

- `subset(mg, samples = c("a", "b"), view = )` picks which children
  participate.
- `samples =` on a transform defines per-child coordinate frames for
  cross-sample layouts.

Together they build a “place these samples together, filtered to cluster
3” recipe without touching the underlying data:

``` r

mg <- subset(mg, leiden_clus == 3, samples = c("visium_a", "visium_b"),
    view = "cluster3")

# visium_a stays put; visium_b moves next to it
mg <- spatShift(mg, dx = 8000, dy = 0, space = "side_by_side",
    samples = "visium_b")

getSpatialLocations(mg, view = "cluster3", space = "side_by_side")
#> Getting values from [cell][rna] cell metadata
#> $visium_a
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 108 3 
#> preview   :
#>    sdimx sdimy            cell_ID
#>    <int> <int>             <char>
#> 1:  5684 -4005 AACGCGGTCTCCAGCC-1
#> 2:  3964 -4125 AACGTCAGACTAGTGG-1
#> 3:  3826 -4125 AAGGCGCGTAAAGCTT-1
#> 
#> ranges:
#>      sdimx sdimy
#> [1,]  3069 -5083
#> [2,]  6096 -3167
#> 
#> 
#> $visium_b
#> An object of class spatLocsObj : "raw"
#> spat_unit : "cell"
#> provenance: cell 
#> dimensions: 108 3 
#> preview   :
#>    sdimx sdimy            cell_ID
#>    <num> <num>             <char>
#> 1: 13684 -4005 AACGCGGTCTCCAGCC-1
#> 2: 11964 -4125 AACGTCAGACTAGTGG-1
#> 3: 11826 -4125 AAGGCGCGTAAAGCTT-1
#> 
#> ranges:
#>      sdimx sdimy
#> [1,] 11069 -5083
#> [2,] 14096 -3167
```

The view’s surviving cell IDs are computed once across the gmulti;
per-child getter calls downstream reuse that cache.

## Design notes

- **Recipes, not state.** *Resolving* a view doesn’t mutate the gobject
  — you pass the name to a getter per-call. Recording a step is the one
  write, and it touches only the recipe, never the data.
- **Plain steps in a class.** The recipe object is S4 — it is the handle
  `[`, the builder verbs and
  [`as.list()`](https://rdrr.io/r/base/list.html) act on — but every
  step inside it is a plain tagged list. That is what lets a recipe
  survive [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and reach a
  parallel worker: the predicate is stored deparsed with its
  environment’s values substituted in, and a crop region is stored as
  WKT rather than a terra pointer.
- **Lazy.** On a disk-backed gobject (via `gsource`), view predicates
  and spatial crops are pushed into the backend’s lazy query plan. The
  actual data pull happens only when a downstream call collects.
- **Composable by name.** Chain
  [`subset()`](https://rdrr.io/r/base/subset.html) /
  [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  calls against the same `view =` to compose; the steps intersect.
  Cross-*view* composition (combining two named views) is reserved for a
  future release.
- **Sample-scoped transforms.** On a `giottoMulti`, `samples =` scopes
  each space step to named children, so one space carries different
  frames for different children.

## See also

- [`vignette("giottoMulti", package = "GiottoClass")`](https://giotto-suite.github.io/GiottoClass/dev/articles/giottoMulti.md)
  — working with several samples, including views and spaces on a
  `giottoMulti`.
- `vignettes/articles/design_view_space.Rmd` — why the subsystem is
  shaped this way, and what is deliberately left undone.
- `vignettes/articles/design_gmulti.Rmd` — the multi-sample container
  that view and space integrate with most deeply.
- [`?giottoView`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoView.md)
  — slotted view accessors.
- [`?giottoSpace`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace.md)
  — slotted space accessors.
