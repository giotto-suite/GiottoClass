# S4 giottoMulti

Container for multiple `giotto` objects whose spatial information is
kept separate (one space per child) but whose expression-space analysis
is shared across all cells.

## Value

giottoMulti object

## Slots

- `objects`:

  named `list` of `giotto` objects (children)

- `id_map`:

  `list` with elements `cells` and `feats`, each a `data.table` mapping
  `(object, local_id) -> global_id`. This is an identity **registry**
  covering every child ID, and is never narrowed.

- `id_sig`:

  `list` of per-child ID length-signatures. Drives id_map cache
  invalidation in `initialize()`.

- `mapping`:

  `list` declaring how child-level content federates up to gmulti-level
  handles. Three named entries — `spat_unit`, `feat_type`, and `values`
  (expression matrix names; flat, not per-universe) — each a list of
  named character vectors mapping `sample -> child-level name`. Every
  entry keys every sample; `NA_character_` is the deliberate-skip
  sentinel ("this sample does not contribute to this handle").
  Auto-discovered at construction; edited via
  [`gmultiMapping()`](https://giotto-suite.github.io/GiottoClass/dev/reference/gmultiMapping.md).

- `groups`:

  named `list` of character vectors declaring that a name refers to
  several samples at once. Usable anywhere a sample name is, so no
  `samples =` formal changes. Entries may name other groups (resolved
  recursively); membership is expanded at **resolution**, not at
  registration, so a group tracks the current child population. Edited
  via
  [`gmultiGroup()`](https://giotto-suite.github.io/GiottoClass/dev/reference/gmultiGroup.md).
  Unlike every other registry here, `@groups` is **not** reset when the
  child population changes — see the note in `initialize()`.

- `expression`:

  shared expression matrices (rows = union of features, cols = global
  cell IDs)

- `expression_feat`:

  available feature types

- `cell_metadata`:

  shared cell metadata (one row per global cell ID)

- `feat_metadata`:

  shared feature metadata (one row per global feature)

- `cell_ID`:

  shared cell ID lists (global IDs). Records active narrowing from stage
  2; `NULL` means unfiltered.

- `feat_ID`:

  shared feature ID lists (global IDs), same contract.

- `dimension_reduction`:

  shared joint dim-reductions (PCA, UMAP, harmony)

- `nn_network`:

  shared joint NN graphs

- `spatial_network`:

  **cross-sample** spatial networks, nested `spat_unit -> name` — the
  same shape as `giotto@spatial_network`. Both endpoints of every edge
  are `sample::id` globals, so an edge may span samples, which is why
  these cannot live on a child whose slot knows only its own cell IDs.
  Per-sample networks are *not* here; they stay on the children.

  A network built in a coordinate frame takes that frame's name by
  default, so two frames do not collide, and carries the frame in its
  `@parameters$space`. See `adr/0006`.

  **This is the only multi-level spatial slot, and the test is whether
  the artifact can be decomposed into per-sample pieces.** An edge
  cannot: its endpoints are in two samples and no per-sample
  representation of it exists. Locations, polygons, points and images
  all can — each one belongs to exactly one sample — so they stay on the
  children, where the owning sample is *where the thing lives* rather
  than a prefix on a string. See the spatial setters below.

- `spatial_enrichment`:

  shared spatial enrichment results

- `multiomics`:

  shared multi-omics info

- `instructions`:

  giotto-style instructions

- `parameters`:

  analysis parameters (mirrors `giotto@parameters`)

- `versions`:

  package versions

- `misc`:

  miscellaneous

- `source`:

  on-disk source / project manager (e.g. GiottoDisk::gDirSource) where
  multi-level shared-domain artifacts live. `NULL` for in-memory multis.
  Children may carry their own per-sample sources; multi-level slots
  (shared `@expression`, `@dimension_reduction`, `@nn_network`) write to
  this one.

- `view`:

  named `list` of view recipes. Reserved for stage 4.

- `spaces`:

  named `list` of coordinate-frame recipes. Reserved for stage 4.
