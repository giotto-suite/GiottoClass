# Changelog

## GiottoClass 0.6.0

### new

- [`hnswKNN()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hnswKNN.md)
  restored to GiottoClass, so `createNearestNetwork(engine = "hnsw")`
  works again. It had errored with
  `'hnswKNN' is not an exported object from 'namespace:GiottoDisk'`
  since 2026-08-11, when {GiottoDisk} removed the function intending to
  move it here and the move did not land, leaving `.nn_search()` calling
  a function that existed in neither package. Requires (Suggests);
  `engine = "dbscan"` remains the default and is unchanged.
- `ef` and `n_threads_build` are now exposed on
  [`kNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/kNNNetworkParam-class.md),
  [`sNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sNNNetworkParam-class.md)
  and
  [`createNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNearestNetwork.md)
  rather than only reachable through `...`. Both apply to
  `engine = "hnsw"` and are ignored by `"dbscan"`, so engines can be
  swapped without changing the call.
  - `ef` (default `200`, was `50`) is the recall/speed dial. Measured on
    a 158,662-cell Xenium sample at `k = 30`: `ef = 50` reproduced
    99.225% of the exact network’s undirected edges, `ef = 200`
    reproduced 99.995%, for 2.30s against 2.83s.
  - `n_threads_build` (default `1`) makes the search reproducible. Only
    the index build is nondeterministic – concurrent insertion makes the
    graph depend on thread interleaving, while the search is read-only
    and deterministic at any thread count. With a parallel build, two
    runs of a seeded Leiden gave ARI 0.9368-0.9655; building on one
    thread they are identical (ARI 1.000000). Costs 2.82s -\> 11.8s,
    still 6.8x faster than the 79.85s exact
    [`dbscan::kNN()`](https://rdrr.io/pkg/dbscan/man/kNN.html), with
    accuracy unchanged (recall 0.999980). Set to `NULL` to inherit
    `n_threads` and trade reproducibility for speed while exploring.
  - An `engine = "auto"` that selects by dataset size is planned; for
    now prefer `"dbscan"` on small data, where it is both exact and
    faster.
- `giotto` class gains a `source` slot for attaching a
  `gsource`-inheriting backend manager (see {GiottoDisk}).
- [`createGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)
  gains a `backend` param: accepts a filepath or a `gsource` object.
  - filepath is converted to a `GiottoDisk::gDirSource` automatically.
  - if `backend = NULL` – the usual in-memory path is used.
- [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  and
  [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  now delegate to `GiottoDisk::snapshotSave()` /
  `GiottoDisk::snapshotLoad()` when a `gsource` backend is attached to
  the object. `foldername` and `dir` params are ignored in that case.
- On-disk persistence via `GiottoDisk::sourceWrite()` is now triggered
  inside `set_expression_values()`, `set_polygon_info()`, and
  `set_feature_info()` when a `gsource` backend is present.
- [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md)
  is now an S4 generic with method dispatch on `matrix`, `spatLocsObj`,
  `dimObj`, and `giotto` inputs.
- `networkParam` virtual class with concrete constructors
  [`kNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/kNNNetworkParam-class.md),
  [`sNNNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sNNNetworkParam-class.md),
  and
  [`delaunayNetworkParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/delaunayNetworkParam-class.md)
  configure
  [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md)
  calls. The legacy `type` string arg is superseded by passing a
  `*NetworkParam` object.
- [`spatRelate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatRelate.md)
  generic — filter-form complement to
  [`relate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/relate.md):
  returns `x` narrowed by a spatial predicate rather than a relation
  matrix. Eager method on `(giottoSpatial, giottoSpatial)` wraps
  `relate() + subset`; the on-disk lazy form lives in GiottoDisk via
  methods on `parquetGeomBase`.

### changes

- `h5_file` param in
  [`createGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_giotto.md)
  is deprecated; use `backend` instead.
- `overlapInfo` class exported as an extension point.
- [`updateGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoObject.md)
  now upgrades pre-0.6.0 objects to initialize the new `source` slot,
  and migrates `spatialNetworkObj` / `nnNetObj` to the new igraph-based
  storage (see breaking changes).
- [`createNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNearestNetwork.md),
  [`createSpatialDelaunayNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialDelaunayNetwork.md),
  and
  [`createSpatialKNNnetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createSpatialKNNnetwork.md)
  are now thin wrappers over
  [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md).
  Behavior is preserved.

### breaking changes

- Network subobject storage migrated from `data.table` to `igraph`:
  - `spatialNetworkObj`: `@networkDT` → `@network` (igraph),
    `@networkDT_before_filter` → `@unfiltered` (igraph)
  - `nnNetObj`: `@igraph` → `@network`
  - [`updateGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoObject.md)
    migrates serialized pre-0.6.0 objects. The same migration runs
    on-load via [`initialize()`](https://rdrr.io/r/methods/new.html) so
    legacy subobjects passed to setters
    ([`setSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setSpatialNetwork.md),
    [`setNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setNearestNetwork.md))
    are upgraded transparently.
- [`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md)
  `output` choices changed: `"networkDT_before_filter"` →
  `"unfiltered"`; new option `"igraph"` returns the underlying graph
  directly.
- Removed exported helpers `convert_to_full_spatial_network()` and
  `convert_to_reduced_spatial_network()`. The edge table is now an
  igraph; use `igraph::as_data_frame(net, what = "edges")` if a
  data.table form is needed.
- Geometric-transform methods
  ([`flip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md),
  [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md),
  [`t()`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md),
  [`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md),
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md),
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md))
  are no longer defined on `spatialNetworkObj`. Graph topology is
  invariant under these transforms; gobject-level walkers skip the
  spatial-network slot.
- `create_average_detection_DT()` removed. No callers remained in the
  suite once Giotto’s gini markers moved onto
  `analyzeData(x, analyzeParam("feat_stats"), groups = )`, whose
  `perc_cells` column is the same statistic.
  [`create_average_DT()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_average_DT.md)
  is retained because
  [`create_cluster_matrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_cluster_matrix.md)
  needs it and GiottoClass cannot depend on Giotto, but it duplicates
  that verb’s `mean_expr` and should not be used in new code.

### bug fixes

- [`create_average_DT()`](https://giotto-suite.github.io/GiottoClass/dev/reference/create_average_DT.md)
  now selects each group’s cells by `cell_ID` rather than by position.
  It fetches the expression matrix and the cell metadata independently,
  and nothing guarantees the two share a cell order. Where they
  diverged, cells were labelled with another cell’s group. **Results
  will change for affected objects**; they were wrong before.
- fix “unused argument (ids = FALSE)” when subsetting a `giottoPolygon`
  object
- skip 0-entry `giottoPoints` in subset paths
- documentation fix in `methods-extract`

### enhancements

- [`addCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addCellMetadata.md)
  and
  [`addFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addFeatMetadata.md)
  now auto-detect a `cell_ID` / `feat_ID` column on `new_metadata`
  (including the column auto-added from a named vector) and route
  through key-based merge regardless of the caller’s `by_column` value.
  Positional `cbind` is fragile when input row order does not match
  metadata row order; the key-based path is safe by construction.
  Positional input without a key column still works for backwards
  compatibility but now emits a warning so callers can opt in to safe
  alignment.
- Slot-accessor S4 generics widened with contract-stable formals
  (`spat_unit`, `feat_type`, `name`, `polygon_name`) so IDE autocomplete
  surfaces them past `gobject`. Setter method defaults for `name`
  (`"raw"`, `"pca"`, `"sNN.pca"`, `"enrichment"`, `"cell"`) moved out of
  the formals and into an explicit body-side fallback after
  `read_s4_nesting()`, allowing `name` on the generic without breaking
  the “user-supplied vs subobject-derived” resolution.
  `match.call`-based detection replaced with plain `is.null(name)`
  throughout.
- `getExpression` adopts `name` as the canonical formal alongside
  `values` (now a back-compat alias); they error if both supplied and
  differ.
- accessor generic formals aligned to one shared contract. Three
  generics deviated from it, which mattered once the formals moved onto
  the generics (S4 requires methods to match the generic’s shared formal
  names and order, so the odd ones out could not be written against the
  common contract): `setMultiomics(result=)` and
  `setGiottoImage(image=)` are now `x` like every other setter, and
  `getPolygonInfo(polygon_name=)` is now `name`. The old names remain as
  deprecated aliases via `deprecate_param()` and continue to work with a
  warning. Positional calls are unaffected. Following the existing
  convention (`calculateOverlap(spatial_info)`,
  `getExpression(values)`), the aliases live on the methods only and
  reach them through the generic’s `...`, so dispatch signatures are
  unchanged.
- `image_type` formal removed from
  [`getGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getGiottoImage.md),
  [`setGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiottoImage.md),
  [`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plotGiottoImage.md),
  and
  [`distGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/distGiottoImage.md).
  The param had been deprecated for a long time and was a no-op in all
  four: the accessors never read it,
  [`plotGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plotGiottoImage.md)
  overwrote any supplied value by inspecting the class of the fetched
  image, and
  [`distGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/distGiottoImage.md)
  accepted only its default `"largeImage"`. Image class is determined
  from the object itself. Note this does not affect the `img_type`
  argument of
  [`list_images()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_images.md)
  /
  [`list_images_names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/list_images_names.md),
  which is a working filter and is retained.

## GiottoClass 0.5.1 (2026/05/14)

### changes

- deprecated `area()` in favor of
  [`expanse()`](https://giotto-suite.github.io/GiottoClass/dev/reference/expanse.md)
- [`createExprObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createExprObj.md)
  no longer coerces to exotic matrix formats – `expression_matrix_class`
  param is deprecated
  - backed matrices (`HDF5Array`, `dbMatrix`, `IterableMatrix`) must be
    pre-constructed and directly passed to be used.
- added superseded note to
  [`createGiottoImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoImage.md)
  documentation
- [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  and
  [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  param harmonization
- refactor of
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  and
  [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
- code reorganization for `classes.R`

### new

- [`aggregateFeatures()`](https://giotto-suite.github.io/GiottoClass/dev/reference/aggregateFeatures.md)
  giotto object wrapper function for running
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  and
  [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
- `overlapPointDT()` and `overlapIntensityDT()` classes to store
  overlaps relationships efficiently and help with aggregation pipeline
- `giottoBinPoints` class for efficient binned spatial points
- `rbind` method for `giottoPoints`
- `affine2d` class is now exported

### bug fixes

- [`overlaps()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlaps-generic.md)
  will now properly find image overlaps
- fix a naming bug when exporting images during save
- `SpatVector` -\> `data.table` coercion no longer returns empty when it
  has no attributes

### enhancements

- [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  for `giottoLargeImage`/`giottoAffineImage` is now lazy by default —
  uses
  [`terra::window()`](https://rspatial.github.io/terra/reference/window.html)
  instead of materializing a crop unless `write = TRUE` or a `filename`
  is given
- `giottoPoints`
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  gains a `sigma` param for Gaussian smoothing of rasterized density;
  `count = TRUE` (replaces `dens` param) is now the default
- image plotting rework – more params exposed, better defaults

## GiottoClass 0.4.12 (2025/12/12)

### bug fixes

- [`seuratToGiottoV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/seuratToGiottoV5.md)/[`giottoToSeuratV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV5.md)
  updated for `layer` param (replacing `slot`)

### enhancements

- automatic checking for `"count"` column in feature info

### new

- `misc` slot for storing unstructured data

### enhancements

- escape hatch for gobject initialize checking. Set option
  `"giotto.init_check_severity"` to `"stop"` (default), or `"warning"`
  depending on needs.

## GiottoClass 0.4.10 (2025/09/30)

### bug fixes

- fix bug in spatial grid getter
  [\#1193](https://github.com/drieslab/Giotto/issues/1193) by RunBelief

### enhancements

- improvements to
  [`tif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
  and
  [`to_simple_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)

## GiottoClass 0.4.9 (2025/07/07)

### bug fixes

- fix irregular default x padding/shift behavior
  [\#1140](https://github.com/drieslab/Giotto/issues/1140) by rbutleriii

## GiottoClass 0.4.8 (2025/06/17)

### new

- [`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md)
  for label proportions calculation from table, network neighbors, and
  polygon selections
- [`clusterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/clusterData.md)
  generic for {bluster} integration

### changes

- [`calculateSpatCellMetadataProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateSpatCellMetadataProportions.md)
  now deprecated in favor of
  [`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md)
- [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  2nd arg has been changed to `feats`

## GiottoClass 0.4.7 (2025/05/06)

### new

- `spatIDs()<-` for `giottoPolygon`
- [`combineGeom()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  and
  [`splitGeom()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combine_split_geoms.md)
  for `giottoPolygon`
- [`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md)
  generic and `processParam` class
- `svkey` metaprogramming object for storing `spatValue()` parameters
  for later eval.

### bug fixes

- fixes and updates for {spatialdata} and {anndata} interoperability.
- fix bug introduced in 0.4.6 with
  [`shear()`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  for `giottoPolygon`.
- fix {magick} `giottoAffineImage` realization when extent does not
  match the image dims ratio.
- fix `ext<-()` for `spatLocsObj`
- fix `ext<-()` for `giottoAffineImage`
- fix external affine matrix compatibility.
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  now has `pre_multiply` param to switch between working with affine
  matrices defined for either pre or post-multiply. Pre is the general
  convention, but Giotto internally uses post. This will be addressed in
  a later update.
- fix
  [`giottoToSeuratV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV5.md)
  selection of a default image to use
- replace internal usage of deprecated create_spat_net_obj -\>
  createSpatNetObj and set_spatialNetwork -\> setSpatialNetwork when
  calculating spatial networks.
- fix
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  not preserving attributes from `data.table` inputs
- fix
  [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  error when a non-expected reticulate environment is already activated
  in the session
- fix
  [`createGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoLargeImage.md)
  and
  [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  to align with {terra} `v1.8-21` `rast(noflip = TRUE)`
  [\#1102](https://github.com/drieslab/Giotto/issues/1102) by
  StevenWijnen and rbutleriii
- add fallback for when attributes do not match number of geometries in
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  so that poly_ID col is not dropped
- fix
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)when
  raster aggregation finds polygons with no values
- fix
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  dispatch on `character` so that it can access poly cleanup params
- fix incorrect `giottoInstructions` class in older objects now possible
  via
  [`updateGiottoObject()`](https://giotto-suite.github.io/GiottoClass/dev/reference/updateGiottoObject.md)
- Remove imports on deprecated {terra}
  [`convHull()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md),
  [`minRect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md),
  [`minCircle()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md),
  in favor of
  [`hull()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  usage [\#1153](https://github.com/drieslab/Giotto/issues/1153) by
  demographix
- Remove import on {terra} `area()`, define as new generic from
  {GiottoClass}
- fix
  [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  issue when there are multiple polygons and some only some of them have
  created centroids
  [\#304](https://github.com/drieslab/GiottoClass/issues/304)
- fix `joinGiottoObjects` polygon joins when there is more than one set
  of polygons
  [\#305](https://github.com/drieslab/GiottoClass/issues/305)

### changes

- `remove_background_poly` now defaults to `TRUE` during polygon
  ingestion
- move {magick} from imports to suggests
- {terra} `>=v1.8-21`
- deprecate
  [`spatQueryGiottoPolygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
  in favor of more general
  [`spatQuery()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
- deprecate
  [`ometif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
  in favor of
  [`tif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
- deprecate
  [`ometif_to_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)
  in favor of
  [`to_simple_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)

### enhancements

- `[[` can now be used to select channels in
  `giottoLargeImage`-inheriting objects
- [`XY()`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  replacement function for `SpatVector` now has `geomtype` param in case
  of `"none"` geometries
- `negate` param for negative selection in
  [`sliceGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sliceGiotto.md)
- [`spatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  and
  [`featType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  method for `giotto` to find existing spatial units and feature types
- expose `make_valid` param and `...` passing for
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  `data.frame` method
- [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  `part_col` param for generating multipolygons from `data.frame-like`
  inputs.
- [`combineCellData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/combineCellData.md)
  `ext`, `xlim`, `ylim` cropping. (also background poly removal in case
  of cropping artefacts)
- large improvements to anndata and spatialdata converters (see
  [\#294](https://github.com/drieslab/GiottoClass/pull/294))
- `spatLocsObj` can now be created from `numeric` xy pairs and xyz
  triplets
- improvements to
  [`spatQuery()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatQuery.md)
- add support for qptiff in
  [`tif_metadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tif_metadata.md)
  and
  [`to_simple_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)
- [`as.matrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  for
  [`nnNetObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/nnNetObj-class.md)
  [\#262](https://github.com/drieslab/GiottoClass/issues/262)

## GiottoClass 0.4.6 (2025/01/17)

### bug fixes

- fix
  [`gefToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/gefToGiotto.md)
  gene column reading
  [\#255](https://github.com/drieslab/GiottoClass/pull/255) by
  cmubioinformatics
- fix `plot(add = TRUE)` for adding on to rasterized point plots
- fix
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  when there are duplicate poly_IDs
- fix
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  `giottoPolygon`, `giottoAffineImage` method. (The `giotto`, `missing`
  method still needs work)
- fix
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  `giottoPolygon`, `giottoLargeImage` method that locked `name_overlap`
  to be
  [`objName()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  of `y`
- fix poly_ID generation when
  [`terra::makeValid()`](https://rspatial.github.io/terra/reference/is.valid.html)
  increases number of polys
- fix `giottoPoints`, `giottoPolygon`
  [`as.data.table()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.data.table.md)
  conversion when [`row()`](https://rdrr.io/r/base/row.html) = 0

### new

- [`names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/names.md)
  and `names<-()` for `giottoLargeImage` inheriting objects to name
  image layers

### enhancements

- `make_valid` param for
  [`createGiottoPolygonsFromDfr()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  and
  [`createGiottoPolygonsFromGeoJSON()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)

## GiottoClass 0.4.5 (2024/12/09)

### enhancements

- `spatUnit()<-` and `featType()<-` `list` methods
- [`set_default_spat_unit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_default_spat_unit.md)
  and
  [`set_default_feat_type()`](https://giotto-suite.github.io/GiottoClass/dev/reference/set_default_feat_type.md)
  now look for defaults when given `NA_character_` inputs as well.
- [`update_giotto_params()`](https://giotto-suite.github.io/GiottoClass/dev/reference/update_giotto_params.md)
  can now be turned off with `options("giotto.update_param" = FALSE)`

### bug fixes

- fix
  [`giottoToSeuratV5()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSeuratV5.md)
  Interoperability for Xenium Image
- fix
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  when no attributes information is provided
- fix
  [`createGiottoPolygonsFromGeoJSON()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  reading from json GeometryCollection type inputs

## GiottoClass 0.4.4 (2024/11/14)

### bug fixes

- fix cell metadata desyncing after
  [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
- fix
  [`readExprMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readExprMatrix.md)
  when IDs are numerical barcodes
- fix `giottoAffineImage` not being detected during
  [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  image export step.
- fix `giottoAffineImage`
  [`reconnect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
  method

### enhancements

- [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  now has `include_feat_coord` param. If `FALSE`, transcript coordinates
  will be dropped during saving, which will make the object much less
  memory intensive.
- [`saveGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
  now has a `export_image` param. If `FALSE`, the image will not be
  re-exported during the save process. (They can still be reconnected)

## GiottoClass 0.4.2 (2024/10/30)

### bug fixes

- fix default method setting in
  [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md)
  for “delaunay” networks
- fix y spacing of
  [`makePseudoVisium()`](https://giotto-suite.github.io/GiottoClass/dev/reference/makePseudoVisium.md)

### changes

- [`makePseudoVisium()`](https://giotto-suite.github.io/GiottoClass/dev/reference/makePseudoVisium.md)
  `micron_scale` (multiplicative scalefactor to get micron scaled
  values) supercedes `micron_size` which used the inverse.

## GiottoClass 0.4.1 (2024/10/28)

### new

- [`buffer()`](https://giotto-suite.github.io/GiottoClass/dev/reference/buffer.md)
  for `giottoPolygon`, `giottoPoints`, `spatLocsObj`. Default is to crop
  by voronoi borders with
  [`settleGeom()`](https://giotto-suite.github.io/GiottoClass/dev/reference/settleGeom.md)
- [`settleGeom()`](https://giotto-suite.github.io/GiottoClass/dev/reference/settleGeom.md)
  for `giottoPolygon` and `SpatVector` for finding non overlapping
  borders determined by voronoi

## GiottoClass 0.4.0 (2024/10/27)

### breaking changes

- stop exporting deprecated internal accessors
- terra requirement raised to 1.7.41 for
  [`minCircle()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)

### bug fixes

- fix
  [`dimnames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  for some subobjects
- fix `joinGiottoObject()` for gobjects with only poly and point data
  [\#233](https://github.com/drieslab/GiottoClass/issues/233)
- fix `joinGiottoObject()` for gobjects with image intensity overlaps
  features
- fix subsetting error due to expression `matrix` drop to `numeric` when
  only one cell is left
- `shift_vertical_step` and `shift_horizontal_step` args in
  [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  when numeric now shift by steps based on the dims of the image instead
  of just by the numerical value provided.
- fix feature metadata not being mixedsorted after join
- fix non-inclusive subsetting when not all minmax values are supplied
  to
  [`subsetGiottoLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocs.md)
- fix `giottoAffineImage` loading after being saved

### enhancements

- python packages to install through pip is now settable in
  [`installGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  [\#224](https://github.com/drieslab/GiottoClass/issues/224)
- `giotto` [`initialize()`](https://rdrr.io/r/methods/new.html) and slot
  checking behavior can be toggled now using `'giotto.init'` and
  `'giotto.check_valid'` options.
  [\#946](https://github.com/drieslab/Giotto/issues/946) by rbutleriii
- [`setGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  now only initializes and performs checks once all items are added if a
  `list` input is provided.
- [`instructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  with no args will now call
  [`createGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md).
  You can also supply named args.
- `instructions(gobject, param)` and `instructions(gobject, param)<-`
  will now work for `giottoInstructions` objects for convenience.
- `[`, `[[`, `$`, `$<-`, and
  [`subset()`](https://rdrr.io/r/base/subset.html) for `giotto` see
  [`?GiottoClass::subset_giotto`](https://giotto-suite.github.io/GiottoClass/dev/reference/subset_giotto.md)
- `subset` for
  [`spatIDs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  and
  [`featIDs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
- [`objName()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md),
  [`spatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md),
  [`featType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  generics now return `NA_character_` instead of erroring when used on
  unsupported classes.
- [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  and `ext<-()` can now be used to get and set extent of `affine2d`
- [`rownames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md),
  [`colnames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/row-plus-colnames-generic.md),
  [`dimnames()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dimnames.md)
  for `giotto`
- [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  can get values from multiple spatial units.
- [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  now works with anything
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  can read
- [`createGiottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoLargeImage.md)
  now works with anything
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  can read

### new

- [`sliceGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/sliceGiotto.md)
  for pulling out specific spatial units and feature types as
  independent `giotto` objects
- [`splitGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/splitGiotto.md)
  for splitting a Giotto object into a list of Giotto objects based on a
  cell metadata column
- [`as.list()`](https://rdrr.io/r/base/list.html) method for `giotto` to
  dump the data as a list of subobjects
- [`XY()`](https://giotto-suite.github.io/GiottoClass/dev/reference/XY.md)
  and `XY<-()` for accessing and setting coordinate values of subobjects
  as `matrix`
- terra
  [`convHull()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md),
  [`minRect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md),
  [`minCircle()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hull.md)
  for Giotto spatial vector classes
- `area()` for `SpatVector` and `giottoPolygon`

## GiottoClass 0.3.5 (2024/08/28)

### breaking changes

- [`set_giotto_python_path()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  will now also initialize python env to set by default and print which
  python env is active, but otherwise do nothing if any python env has
  already been initialized.
- deprecated
  [`readGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/readGiottoInstructions.md),
  [`showGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/showGiottoInstructions.md),
  [`changeGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/changeGiottoInstructions.md),
  [`replaceGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/replaceGiottoInstructions.md)
  in favor of
  [`instructions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_instructions.md)
  generic

### bug fixes

- intensity images now automatically scale to estimated highest value
- `giottoPolygon`
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  default `max_poly` raised to `1e6`
- `giottoInstructions` no longer lose class when specific params are
  replaced
- [`ometif_to_tif()`](https://giotto-suite.github.io/GiottoClass/dev/reference/to_simple_tif.md)
  now checks for *imagecodecs* package as well
- [`anndataToGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/anndataToGiotto.md)
  and `giottoToAnndata` now check for *anndata* package as well.
- fix
  [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
  `"z_stack"` join method
- fix error in documentation
  [\#214](https://github.com/drieslab/GiottoClass/issues/214) by
  shaojunyu
- fix error in
  [`installGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  [\#1006](https://github.com/drieslab/Giotto/issues/1006) by
  13954380607

### enhancements

- [`print()`](https://rdrr.io/r/base/print.html) method for
  `giottoInstructions`
- [`rbind()`](https://rdrr.io/r/base/cbind.html) for `spatLocsObj`

## GiottoClass 0.3.4 (2024/08/04)

### bug fixes

- hotfix anndata matrix support
  [\#216](https://github.com/drieslab/GiottoClass/issues/216) by
  wwang-chcn

## GiottoClass 0.3.3 (2024/07/29)

### bug fixes

- fix flipping issue with `giottoAffineImage` for certain affine
  transforms

### enhancements

- `missing` method for
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  instantiates an `affine2d` object

## GiottoClass 0.3.2 (2024/07/26)

### breaking changes

- python environment installation and how it relates to default settings
  such as .condarc may have changed.
- `giottoImage` `name` slot now requires `character` and will not accept
  `NULL`

### bug fixes

- [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  no longer errors with similarly named spat_units or feat_types
  (e.g. “cell” and “new_cell” would previously throw an error)
- fix in
  [`giottoToSpatialExperiment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoToSpatialExperiment.md)
- fix for `giottoToSeuratV5` for cosmx mini dataset
  [\#989](https://github.com/drieslab/Giotto/issues/989) by
  guillermoturiel
- fix issue with prints in `createGiottoCosMxObject()`
  [\#960](https://github.com/drieslab/Giotto/issues/960) by GBeattie

### enhancements

- `verbose` param for
  [`createNearestNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNearestNetwork.md)
- [`checkGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  in addition to full filepaths, also now supports name of environment
  or installation directory
- [`installGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md),
  [`removeGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  now have `conda` param for setting path to conda executable and
  `envname` param for specifying environment by name
- [`installGiottoEnvironment()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_python.md)
  now has `confirm` param for skipping path input checks
- [`t()`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  for `giotto` now affects images as well.

### new

- [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
  for `giottoPolygon`, `giottoPoints`, `spatLocsObj`, `giotto`
- [`shear()`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  for `giottoPoints`, `giottoPolygon`, `spatLocsObj`, `affine2d`
- `affine2d` class for accumulating linear transforms to be used with
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md)
- [`initialize()`](https://rdrr.io/r/methods/new.html), `[`, `$`,
  [`show()`](https://giotto-suite.github.io/GiottoClass/dev/reference/show.md),
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md),
  methods for `affine2d`
- [`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md),
  `rescale`,
  [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md),
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md),
  [`flip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md),
  [`shear()`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md)
  [`t()`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  methods for `affine2d`
- `giottoAffineImage` class for just-in-time affine transformed images
- [`initialize()`](https://rdrr.io/r/methods/new.html), method for
  `giottoLargeImage`
- [`initialize()`](https://rdrr.io/r/methods/new.html),
  [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md),
  [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md),
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md),
  [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md),
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md),
  methods for `giottoAffineImage`
- [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  method for `giottoImage`
- [`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md),
  [`shear()`](https://giotto-suite.github.io/GiottoClass/dev/reference/shear.md),
  [`affine()`](https://giotto-suite.github.io/GiottoClass/dev/reference/affine.md),
  [`flip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md),
  [`t()`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md)
  methods for `giottoAffineImage` and `giottoLargeImage` (which converts
  to `giottoAffineImage`)
- [`as()`](https://rdrr.io/r/methods/as.html) conversion from
  `giottoLargeImage` to `giottoAffineImage`
- `.get_centroid_xy()` internal for getting numeric centroid xy values
  of any object that responds to
  [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
- `.bound_poly()` internal for generating a dummy polygon from the
  extent of any object that responds to
  [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
- `.aff_shift_2d()`, `.aff_shift_2d<-()`, `.aff_linear_2d`,
  `.aff_linear_2d()<-` internals for accessing and manipulating affine
  matrices

## GiottoClass 0.3.1 (2024/05/21)

### bug fixes

- allow passing of additional params with
  [`setGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  with `...`
- [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  can now perform z shifts when start `spatLocsObj` has no z information
- fix bug in
  [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
  after v0.3.0 where it looks for the now non-existent `@largeImages`
  slot
- fix bug in `.update_image_slot()` after v0.3.0 where a NULL
  `@largeImages` slot will result in an error
- fix bugs in
  [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  and
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  methods for `giotto` when setting a default `spat_unit` and
  `feat_type`

### enhancements

- [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
  extent detection and xshift defaults now depend on
  [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  of the gobject instead of any images (when available)
- [`joinGiottoObjects()`](https://giotto-suite.github.io/GiottoClass/dev/reference/joinGiottoObjects.md)
  now has a `dry_run` param for previewing where datasets will be
  spatially located after the join

### new

- [`as()`](https://rdrr.io/r/methods/as.html) conversion from
  `giottoLargeImage` to `array`
- [`as.matrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.matrix.md)
  method for
  [`spatLocsObj()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatLocsObj-class.md)

## GiottoClass 0.3.0 (2024/05/13)

### breaking changes

- deprecation of
  [`reconnect_image_object()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect_image_object.md),
  [`reconnect_giottoImage_MG()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect_giottoImage_MG.md)
  and
  [`reconnect_giottoLargeImage()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect_giottoLargeImage.md)
  internals in favor of simpler
  [`reconnect()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reconnect.md)
  generic
- `giotto` `@largeImage` slot is removed. All images now exist in
  `@images` slot.
- backwards compatibility for S3 `spatialNetworkObj` removed
- Not finding a specific `spatialNetworkObj` with
  [`getSpatialNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/getSpatialNetwork.md)
  is now upgraded to an error instead of returning `NULL` to be in line
  with other accessors.
- backwards compatibility for bare `data.table` spatial coordinates
  information is removed

### bug fixes

- fix
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  params passing for `giottoPolygon` when `type = "centroid"`
- fix
  [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  output for `giottoImage`
- [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md)
  and
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md)
  now also affect gobject attached images
  [\#945](https://github.com/drieslab/Giotto/issues/945) by rbutleriii

### enhancements

- use faster
  [`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html)
  and
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
  instead of
  [`scattermore::scattermoreplot()`](https://rdrr.io/pkg/scattermore/man/scattermoreplot.html)
  for `giottoPoints`
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  method
- [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  `giottoPoints` method now plots density when `dens = TRUE`
- `show_max` param in
  [`density()`](https://giotto-suite.github.io/GiottoClass/dev/reference/density.md)
  and
  [`hist()`](https://giotto-suite.github.io/GiottoClass/dev/reference/hist.md)
  to plot the image object’s `max_window` setting
- [`.identify_background_range_polygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-identify_background_range_polygons.md)
  now finds any polygons larger than a threshold percentage than the
  overall extent of the `SpatVector` input.
- [`ext()`](https://giotto-suite.github.io/GiottoClass/dev/reference/ext.md)
  can now be used with `giotto` objects
  [\#945](https://github.com/drieslab/Giotto/issues/945) by rbutleriii
- `ext()<-` can now be used with `giottoImage`
- `as` conversion from `giottoLargeImage` to `giottoImage`
  (`giottoImage` is sampled)
- [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  works for `spatialNetworkObj`

### new

- new
  [`spatValues()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatValues.md)
  to get specific values from a `giotto` object in `data.table` format
- new `ometif_to_tif` to convert between .ome.tif and .tif
- new
  [`terra::density()`](https://rspatial.github.io/terra/reference/density.html)
  and
  [`terra::hist()`](https://rspatial.github.io/terra/reference/hist.html)
  wrappers for `giottoLargeImage`

## GiottoClass 0.2.3 (2024/03/12)

### bug fixes

- fix `saveGiotto` with `overwrite = TRUE`
  [\#870](https://github.com/drieslab/Giotto/issues/870) by rbutlerii
- fix
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  method param passing for `giottoLargeImage`. Ensure access to terra
  params

### enhancements

- `createGiottoPoints` `data.frame` method can now select which columns
  to use with `x_colname`, `y_colname`, `feat_ID_colname` params
- `giotto` now responds to spatial manipulation generics:
  [`t()`](https://giotto-suite.github.io/GiottoClass/dev/reference/transpose.md),
  [`flip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/flip.md),
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/dev/reference/rescale.md),
  [`spatShift()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatShift.md),
  [`spin()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spin.md)
- [`spatUnit()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  and
  [`featType()`](https://giotto-suite.github.io/GiottoClass/dev/reference/giotto_schema.md)
  are now vectorized
- internal `get_spatial_locations_list()` and
  `get_spatial_network_list()` accessors now accept “:all:” token to get
  all available, ignoring spat_unit

## GiottoClass 0.2.2 (2024/03/01)

### bug fixes

- fix
  [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  IDs being applied out of sync to mask values
- fix
  [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  `character` method dispatch for `raster` inputs
- remove unused `fix_multipart` param in
  [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
- fix `giottoPolygon` ID caching after
  [`rbind()`](https://rdrr.io/r/base/cbind.html)

### enhancements

- [`createGiottoPolygonsFromMask()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPolygon.md)
  now has `ID_fmt` param for finer control of automatic `poly_ID`
  generation
- `.flip_spatvect()` internal for flipping `SpatVector` across arbitrary
  x and y vals

## GiottoClass 0.2.1 (2024/02/28)

### breaking changes

- `giotto` slot `versions` supercedes `OS_platform`. Used for tracking
  GiottoClass version.

### bug fixes

- fix `subsetGiotto` unused `on` argument
- fix `giotto` object saving when image intensities overlaps data are
  present.
- fix `exprObj`
  [`show()`](https://giotto-suite.github.io/GiottoClass/dev/reference/show.md)
  for small matrices
- fix `giotto`
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  method when working with image intensities data.

### new

- [`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md)
  hub function for creation of Giotto NN and spatial networks directly
  from matrices. Mainly for developers and advanced users.
- [`edge_distances()`](https://giotto-suite.github.io/GiottoClass/dev/reference/edge_distances.md)
  for calculating euclidean distances from numeric m x n `matrix`
  (nodes) and a `data.table` with *from* and *to* cols that define node
  connections.

### enhancements

- [`addCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addCellMetadata.md)
  and
  [`addFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addFeatMetadata.md)
  now support merging on the names of provided vector and factor data
  with metadata *cell_ID*/*feat_ID*.

## GiottoClass 0.1.3 (2024/01/12)

### bug fixes

- fix unexpected sorting in
  [`addCellMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addCellMetadata.md)
  and
  [`addFeatMetadata()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addFeatMetadata.md)
  [\#853](https://github.com/drieslab/Giotto/issues/853) by rbutleriii

### new

- `init_gobject` param in
  [`loadGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/loadGiotto.md)
  to control whether object initialization is also performed after load
- vignette for image tools

### enhancements

- ID sorts now use
  [`gtools::mixedsort()`](https://rdrr.io/pkg/gtools/man/mixedsort.html)
  [\#853](https://github.com/drieslab/Giotto/issues/853) by rbutleriii
- more subobjects respond to `colnames`, `rownames`, `dimnames`
- [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  and
  [`show()`](https://giotto-suite.github.io/GiottoClass/dev/reference/show.md)
  now handle 3D `spatLocsObj`

## GiottoClass 0.1.2 (2024/01/02)

### Added

- Added: `max_window` and `colors` slots to `giottoLargeImage`. Use
  `GiottoClass:::.update_giotto_image()` to update outdated objects.
- Added:
  [`.bitdepth()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-bitdepth.md)
  internal function to detect image bitdepth from sampled values
- Added: re-export
  [`getMonochromeColors()`](https://giotto-suite.github.io/GiottoUtils/reference/getMonochromeColors.html)
  from *GiottoUtils*
- Added: `giottoPolygon`, `giottoLargeImage` method for
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
- Added: vignette for working with spatial classes
- Added: `output` param to
  [`.spatraster_sample_values()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-spatraster_sample_values.md).
  Can now return as sampled `data.frame`, `array`, `magick`, `EBImage`

### bug fixes

- param fixes in raster
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  workflows

## GiottoClass 0.1.1 (2023/12/16)

### Breaking Changes

- Removed:
  [`getRainbowColors()`](https://giotto-suite.github.io/GiottoUtils/reference/getRainbowColors.html)
  to *GiottoUtils*
- Removed: `get_prev_fname()` and `get_args()` to *GiottoUtils*
- Removed: `aggregateStacksPolygonsOLD()`

### Added

- Added: `.gstop()` for GiottoClass specific errors
- Added:
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  method for `dimObj`
- Added: [`ncol()`](https://rdrr.io/r/base/nrow.html) and
  [`nrow()`](https://rdrr.io/r/base/nrow.html) methods for `dimObj`
- Added: `dimObj` additional info now accessible using `$` and `$<-`
- Added: `spatEnrObj` cols now accessible using `$` and `$<-`
- Added:
  [`evaluate_input()`](https://giotto-suite.github.io/GiottoClass/dev/reference/evaluate_input.md)
  as exported wrapper for `evaluate` functions
- Added:
  [`as.polygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md)
  `data.frame` method for correctly formatted data.tables (replaces
  internal `dt_to_spatvector_polygon()`)
- Added:
  [`as.polygons()`](https://giotto-suite.github.io/GiottoClass/dev/reference/as.polygons.md)
  `data.frame` method for correctly formatted data.tables (replaces
  internal `dt_to_spatvector_points()`)
- Added: autocompletes for `$` with `spatLocsObj`, `spatEnrObj`,
  `dimObj`, `cellMetaObj`, `featMetaObj`, `giottoPolygon`,
  `giottoPoints`
- Added: `toplevel_params` param to
  [`subsetGiottoLocs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/subsetGiottoLocs.md)
- Added: `spin`, `spatShift`, `rescale` methods for `data.frame`

### Changes

- Fixed: Provenance now not accidentally created as a list in
  [`addSpatialCentroidLocations()`](https://giotto-suite.github.io/GiottoClass/dev/reference/addSpatialCentroidLocations.md)
- Changed: `giottoPolygon`
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  now automatically switches to centroid plotting with more than 1e4
  polys
- Changed: Package internal functions now have `.` prefix

## GiottoClass 0.1.0 (2023/11/29)

### Breaking Changes

- Giotto subsetting rework

### Added

- Added: `terraVectData` as virtual parent class for `giottoPolygon` and
  `giottoPoints` classes
- Added: `$` and `$<-` methods for `terraVectData`
- Added: `ext<-()` method for `giottoPolygon`, `giottoPoints`
- Added:
  [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  method for `giottoLargeImage`, `giottoPoints`
- Added: `[` subsetting for `giottoPoints` and `giottoPolygon` with
  numerical, logical, and character (by ID)
- Added:
  [`as.sf()`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  and
  [`as.stars()`](https://giotto-suite.github.io/GiottoClass/dev/reference/r_spatial_conversions.md)
  converters for `giottoPoints` and `giottoPolygon`
- Added:
  [`setGiotto()`](https://giotto-suite.github.io/GiottoClass/dev/reference/setGiotto.md)
  generic
- Added: `gap` param to
  [`tessellate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tessellate.md)
  which introduces a variable gap between the polygons tessellated
- Added:
  [`triGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
- Added:
  [`orthoGrid()`](https://giotto-suite.github.io/GiottoClass/dev/reference/generate_grid.md)
- Added: DelayedMatrixStats to suggests
- Added:
  [`calculateOverlap()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateOverlap.md)
  and
  [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoClass/dev/reference/overlapToMatrix.md)
  as generic functions for feature and image overlap aggregations

### Changes

- Improved: performance of gefToGiotto()
- Updated:
  [`spatIDs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  and
  [`featIDs()`](https://giotto-suite.github.io/GiottoClass/dev/reference/spatIDs-generic.md)
  methods for `giottoPolygon` and `giottoPoints` to allow returning
  non-unique IDs
- Added: check for
  [`plot()`](https://giotto-suite.github.io/GiottoClass/dev/reference/plot-generic.md)
  when `giottoPolygon` or `giottoPoints` objects contain no geometries
- Added: warning for
  [`crop()`](https://giotto-suite.github.io/GiottoClass/dev/reference/crop.md)
  when `giottoLargeImage`, `giottoPolygon`, `giottoPoints` objects are
  being cropped with an extent that does not include any information
- Changed: Conversion of
  [`createGiottoPoints()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createGiottoPoints.md)
  to a generic function
- Deprecate: `radius` param in favor of `shape_size` for
  [`tessellate()`](https://giotto-suite.github.io/GiottoClass/dev/reference/tessellate.md)
- Fixed: python
  [`.install_github_link_pip()`](https://giotto-suite.github.io/GiottoClass/dev/reference/dot-install_github_link_pip.md)
  param
- Fixed: python `create_AnnData()` added to `globals.R`
