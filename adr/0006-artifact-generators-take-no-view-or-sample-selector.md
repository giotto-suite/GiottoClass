# 0006. Artifact generators take no `view` and no sample selector; job size comes from the space

- **Status:** Accepted
- **Date:** 2026-09-15
- **Supersedes:** —
- **Superseded by:** —

## Context

The recipe subsystem gave readers three narrowing knobs — `view =` (a recorded
filter/crop chain), `space =` (a coordinate frame), and `samples =` (which
children of a `giottoMulti`). Threading them through the getters was deliberate
and wide: 17 `view =` formals and 30 `space =` formals in this package alone.

Functions that *write* into a slot were never given `view =`, but nothing said
why, and nothing said they could not have one. So the rule held by accident
until `createSpatialNetwork()` grew a `giottoMulti` arm with a parameter spelled
`space` whose documented meaning was *"a character vector of sample names to run
on, or `NULL`/`":all:"` to run on every child"* — a sample selector wearing a
coordinate-frame name.

Three things followed from that, and together they are why this is written down
rather than just fixed:

- It became a **sixth** site validating sample names against
  `names(gobject@objects)` with its own error string, three weeks after five
  others were converged onto `.gm_resolve_samples()`. It was missed by that pass
  precisely because the variable was named `space`, so a registered `@groups`
  name worked in `getCellMetadata(mg, samples = )` and failed here.
- It spelled `":all:"` to mean *all children*, while every other use of that
  token in the package (`spat_unit = ":all:"`, `feat_type = ":all:"`,
  `images = ":all:"`) means *all keys of a nesting axis*. One token, two axes.
- Its per-child dispatch forwarded 19 formals by hand. When upstream added
  `radius` to the signature, the list was not updated and the merge was clean,
  so `createSpatialNetwork(mg, method = "radius", radius = 50)` bound `radius`
  at the container and dropped it on the way to every child.

The deeper problem is what a selector on a writer *means*. A reader that
narrows returns a value the caller can widen again by calling differently. A
writer that narrows leaves rows in a slot, and nothing downstream can recover
which rows were admitted, which were excluded, or on what basis. A
`@spatial_network` built over two of five samples is indistinguishable from one
built over five where three happened to have no edges.

At the same time, `giottoSpace` acquired a genuine notion of job size. The
kind of space says whether its samples **interact**: a `combinedSpace` lays
them out in one coordinate system, so a job over it is one job and
cross-sample distances mean something; a `perSampleSpace` gives each sample
its own copy of the frame, so a job over it is N independent jobs. That is the
missing declaration — the object that can say *whether the samples participate
together* — and it is already persisted next to the artifact.

This is **not** the same axis as whether steps are scoped. Both kinds may
scope a step to named samples: two sections each rotated upright by a
different angle are per-sample, because nothing about that puts them in a
shared coordinate system. Scoping says which samples *move*; the kind says
whether they *interact*. Reading the first as the second is the mistake this
records — it is what made `samples =` look like it could decide the kind.

## Decision

A function that persists an artifact into a slot takes no `view =` and no
sample selector. Its job size is read from the space.

- A **`combinedSpace`** is pulled in one piece: one artifact, cell IDs in
  `sample::id` global form, written to the `giottoMulti`'s joint slot. Its
  membership is every sample its steps name — derived, not stored, so
  there is no slot that can fall out of step with the recipe. It is
  deliberately NOT seeded from the object's children: a membership that
  always equalled `names(@objects)` would restate the object rather than
  declare anything, and growing it key by key would then carry no
  information either. A member that needs no transform of its own — the
  sample at the layout's origin, which is in the layout precisely by not
  moving — is declared with a `member` step, which is why membership
  derived purely from transforms was not enough.
- A **`perSampleSpace`** iterates: N artifacts, child-local IDs, one written
  per child.

Recording onto an unused name declares a **`perSampleSpace`**, and a
`combinedSpace` is declaration-only. `samples =` cannot be the signal either
way, because both kinds accept it — scoping says which samples *move*, not
whether they *interact*.

The free kind is the one that composes under get/set. A per-sample job writes
one artifact per child, which is the shape reading per child hands back, so
content can go out and come back. A combined job writes ONE artifact at the
parent, and there is deliberately nowhere to put per-sample content back at
the parent (`design_gmulti.Rmd`, "What does not live at the parent"), so that round trip
does not close. Declaring that samples share a coordinate system is a real
claim and is made out loud.

*(This reverses the original decision, which made `combinedSpace` the free
kind on the grounds that laying samples out together is overwhelmingly why a
space gets named. That may well be true, but it is an argument about
frequency, and the kind decides job size — so the tie-breaker should be which
default composes, not which is typed more often.)*

**The native frame has no name.** `space = NULL` is it. There was briefly a
`":default:"` sentinel; it is gone, because a name for "no space" is a second
spelling of a value R already has, which every consumer then has to know is
the same thing — and a transform recorded onto the native frame would stop it
being native, so the name could only ever stand for an empty recipe. On a
`giottoMulti` a transform with no `space =` is refused, naming the remedy; on
a plain `giotto` it transforms eagerly, as it always has.

"Sample selector" means any formal whose documented meaning is *which children
to compute over*, whatever it is spelled. A caller who wants a subset of samples
says so by recording a space over them, or by subsetting the container with
`mg[...]`, which produces an object whose artifacts are unambiguous.

Readers keep all three of `view =` / `space =` / `samples =`. The asymmetry is
the decision: narrowing a return value is reversible, narrowing a slot is not.

**A frame is a recorded setting, so an artifact built in one must say which.**
This is the sanctioned exception to the reasoning above, and it is sanctioned
because a frame is *persisted state on the object* rather than a per-call
argument — but the exception is only safe if the artifact carries the frame
out with it. `rescale`, `shear` and a general `affine` change distances, so
`maximum_distance` and `radius` mean different things per frame and `shear`
changes Delaunay topology outright; `spin`, `flip` and `spatShift` are
isometries and change nothing. Which of those a frame is cannot be read off
the result.

So a frame-built artifact records it twice, for two different jobs:

- **the default name gains the frame as a prefix** (`scaled2x_kNN_network`
  against `kNN_network`), which *keys* it — two frames cannot overwrite each
  other, and the method stays readable off either name. An explicit `name =`
  is taken exactly as given.
- **`@parameters$space`** carries it machine-readably. Not `@provenance`:
  that slot answers "which spat_units were aggregated to produce this", a
  different axis, and two of its consumers assume an atomic value — `cat()`
  in `.show_prov()` errors on a list, and the manifest's `as.character()`
  drops the names and reports the frame as a spat_unit.

**A frame may key a `name`. It may never key a `spat_unit`.** These are not
the same kind of key, and the difference decides where a frame is allowed to
appear.

A `name` is local: it distinguishes artifacts *within* one `spat_unit`, so
adding `scaled2x_` to one creates a sibling and nothing else changes. A
`spat_unit` is the object's primary nesting axis — it keys expression, cell
metadata, spatial locations, polygons, every network. Introducing one is
declaring a new population of cells. A frame does not create cells; it moves
the ones that exist. So a `spat_unit` named after a frame would fork the
object into two populations that are the same cells twice, with no accessor
able to say they are the same and every downstream join silently choosing one.

The failure mode is a *default*, not a call. Nothing stops a user from
passing `spat_unit = "atlas"` deliberately, and nothing should — the two
namespaces are separate and a collision between them is the user's to make.
What is forbidden is code that reaches for the active frame's name when it
needs a `spat_unit` and none was given. That is the shape to reject in
review, because it looks like symmetry with the naming rule above and is not.

The consequence for pipelines: **pulling content from a space is what departs
from the native frame, and naming the result is the user's responsibility.**
Reading polygons through a frame, buffering them and writing them back is a
legitimate round trip — the user asked for the frame on the way out and knows
what the result is. What no Giotto method or pipeline may do is close that
loop itself *and* supply the `spat_unit` from the frame. The round trip is
fine; the defaulting is the violation.

Audited 2026-09-16 across GiottoClass, Giotto, GiottoVisuals and GiottoDisk:
no site derives a `spat_unit` from a space, and the single place a frame name
composes into anything is `createSpatialNetwork()`'s default `name`. This
clause exists to keep that true, since there is no runtime check that could —
a guard cannot tell a defaulted `spat_unit` from a deliberately passed one.

**A gobject owns the frames it is asked to work in.** Every public `space =`
takes a **name**, resolved against that object's `@spaces`. `giottoSpace`
handles exist and `.resolve_space()` accepts them, but that is an internal
channel — it is how a `giottoMulti` hands each child the frame narrowed to
itself, since a space is registered on the parent and a child's `@spaces` is
empty. (Forwarding the *name* instead was a live bug:
`getSpatialLocations(mg, space = "atlas")` failed with "'atlas' is not a
registered space" against a multi that plainly had one.)

Letting a caller pass a detached handle would break ownership in two places,
and they are not equally obvious:

- **on a generator**, fatally. A frame-built artifact prefixes the frame's
  name onto its own and stores it in `@parameters$space`. A handle the object
  never registered writes provenance pointing at nothing — a recorded frame
  name that no later lookup can resolve. This is why
  `createSpatialNetwork()`'s `space =` stays `assert_string`, even though the
  resolver one line later would have taken the object.
- **on a reader**, quietly. Nothing is persisted, so it looks harmless — but
  the returned content is in a frame the object cannot name, and the moment
  anyone writes it back the frame is unrecoverable.

The remedy is one line and makes the ownership explicit:
`giottoSpace(x, "<name>") <- sp`. So there is no case where a handle is
*needed* at a public boundary, which is what makes name-only cheap to hold.

## Consequences

- **`createSpatialNetwork(gobject, space = "sample_a")` is a breaking change.**
  `space` keeps its spelling and gains the opposite meaning — a frame name
  resolved against `@spaces`. The old call now fails at frame lookup, which is
  deliberate: a silently renamed argument would be worse than a loud one, and
  the failure is a name that is a sample rather than a frame, so the error can
  say exactly that.
- **`":all:"` goes with the selector rather than being retired separately.**
  It is a *value* in the selector's vocabulary — "every member of the set this
  parameter selects from" — so it can only ever be passed to a selector.
  Remove the parameter and there is nothing left that could accept the token;
  there is no shim to write, because any shim would itself be a selector. The
  token keeps its meaning on the nesting axes (`spat_unit`, `feat_type`,
  `images`), which select keys rather than samples and are a different
  question.
- **Per-child forwarding must not re-list formals.** The hand-listed forward is
  what dropped `radius`. It was first replaced by rewriting the caller's own
  `match.call()`, and then by removing the forward entirely: dispatch moved down
  to `.create_spatial_network_from_param()`, where the `networkParam` is already
  built, so each child is handed an object rather than a replayed call. A built
  param has no formals to forget. The rule generalises — if a per-child dispatch
  needs to enumerate a signature someone else owns, it is sitting too high.
- **The `set*` family lost its `object =` write target, and with it any way
  to reach a child.** This was originally filed as out of scope — `object =`
  named a single child to write into rather than selecting a set to compute
  over, so it was not a selector and the rule above did not reach it. That
  reasoning was about the wrong question. The defect a selector on a writer
  has is that the slot cannot say afterwards which samples it covers, and a
  single-target write has exactly that defect in its sharpest form: it puts
  one sample's artifact in the same slot namespace as an all-sample one, with
  nothing distinguishing them. A `giottoMulti` exists for combined analysis,
  whose outputs are single items pulled from the parent; heterogeneous
  select-sample outputs must not be storable beside all-sample ones. So the
  five spatial setters now write at the multi level and nowhere else, and
  `.gm_set_target()` is gone. Passing `object =` (or `samples =`) is refused
  by name rather than swallowed by `...`. Editing one sample is still
  possible, and now says what it is: `mg[["<sample>"]] <- <edited child>`.
  The one sanctioned per-child write is `createSpatialNetwork()`'s
  `perSampleSpace` path, which assigns into `@objects` internally.
- **Restricting a job now requires declaring a space.** That is a real
  ergonomic cost for the one-off case, accepted because the alternative is an
  artifact whose provenance is unrecoverable. `mg[c("a", "b")]` remains the
  cheap escape and has none of the ambiguity.
- **Every future artifact generator that accepts `space =` owes both records.**
  A generator that takes a frame and neither names for it nor stores it
  produces an artifact that cannot be told apart from one built natively. The
  name prefix is the part that is easy to forget, because omitting it looks
  harmless right up to the second frame.
- **`space =` was previously accepted and silently discarded on a single
  `giotto`.** `createSpatialNetwork(g, space = "scaled2x")` returned a
  native-frame network. That is fixed here, and it is the failure this rule
  is shaped to prevent: the formal existed, looked meaningful, and was not
  wired.
- Revisit if a generator appears whose natural job size is genuinely neither
  per-sample nor per-frame — a cross-frame artifact, say. The rule would then
  need a third case rather than an exemption.

## Alternatives considered

- **Rename `space` to `samples` and route it through `.gm_resolve_samples()`** —
  fixes the sixth-site and `@groups` problems and nothing else. It keeps a
  selector on a writer, so the artifact still cannot say which rows were
  admitted, and it would have made the rule harder to state later because there
  would be a sanctioned example of the thing being forbidden.
- **Forbid `view =` on writers but allow `samples =`** — the two have the same
  defect. A view is a recorded narrowing and a sample list is an ad-hoc one;
  neither is recoverable from the slot afterwards.
- **Let the writer record the narrowing alongside the artifact** — a provenance
  stamp saying which samples were included. This works, and it is what
  `@misc$gmulti$participation` does for assembled expression, but it makes every
  consumer responsible for reading and honouring a stamp. The space already
  carries the declaration and is already persisted; a second authority for the
  same question is the thing to avoid.
- **Keep per-child writes and give the multi no joint slot at all** — leaves
  cross-sample edges with no home, since no child's `@spatial_network` can hold
  an edge between two samples. Adopted for *everything else*, though: a
  multi-level spatial slot earns its place only when the artifact cannot be
  decomposed into per-sample pieces. An edge cannot; a location, a polygon, a
  point and a raster each belong to exactly one sample and stay on the child.
  `@spatial_locs` / `@spatial_info` / `@feat_info` / `@images` were built on the
  multi and then removed on that test — holding per-sample content at the parent
  turns its owning sample from a structure (the child it lives in) into a
  `sample::` prefix every consumer has to remember. See
  `design_gmulti.Rmd`, "What does not live at the parent".

## References

- `R/spatial_structures.R` — `.create_spatial_network_from_param()`, the one
  site all three public doors converge on: where the frame reaches the artifact's
  name and where a `giottoMulti`'s job size is read off the space.
  `.csn_multi()` / `.csn_combined()` are the two job sizes
- `R/gmulti.R` — `.gm_resolve_samples()` (the helper someone reaches for when
  adding a selector), `.gm_reject_write_selector()` (what a setter does when
  someone reaches for one anyway), and the five multi-level spatial slots
  that give a setter somewhere to write
- `R/classes-space.R` — `combinedSpace` / `perSampleSpace`, the interaction
  split this rule depends on, and `.space_steps_for()`, the one resolution
  rule. Scope lives on the step rather than on a per-sample list, so one
  ordered list replays correctly for a sample first named after a broadcast
  step was already recorded
- `vignettes/articles/design_gmulti.Rmd`, "The one exception: joint
  `@spatial_network`" — the joint slot this partially implements
- ADR 0004 — `@network` polymorphism, the other constraint on network writers
