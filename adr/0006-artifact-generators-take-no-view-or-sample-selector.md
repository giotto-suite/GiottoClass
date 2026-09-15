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

At the same time, `giottoSpace` acquired a genuine notion of membership: a
`combinedSpace` names the samples that share one coordinate frame, and a
`perSampleSpace` is an alias for N independent single-occupant frames. That is
the missing declaration — the object that can say *which samples participate,
and whether they interact* — and it is already persisted next to the artifact.

## Decision

A function that persists an artifact into a slot takes no `view =` and no
sample selector. Its job size is read from the space.

- A **`combinedSpace`** is pulled in one piece: one artifact, cell IDs in
  `sample::id` global form, written to the `giottoMulti`'s joint slot.
- A **`perSampleSpace`** — which is what `space = NULL` means — iterates: N
  artifacts, child-local IDs, one written per child.

"Sample selector" means any formal whose documented meaning is *which children
to compute over*, whatever it is spelled. A caller who wants a subset of samples
says so by recording a space over them, or by subsetting the container with
`mg[...]`, which produces an object whose artifacts are unambiguous.

Readers keep all three of `view =` / `space =` / `samples =`. The asymmetry is
the decision: narrowing a return value is reversible, narrowing a slot is not.

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
  what dropped `radius`; it is replaced by rewriting the caller's own
  `match.call()`. Any future per-child dispatch owes the same treatment — an
  enumerated list is a signature someone else owns, copied.
- **The `set*` family's `object =` is not a selector and is out of scope.** It
  is a single-valued write target ("which child owns the object I am handing
  you"), resolved by `.gm_set_target()`. Anyone running this rule across the
  codebase will file the setters as violations otherwise.
- **Restricting a job now requires declaring a space.** That is a real
  ergonomic cost for the one-off case, accepted because the alternative is an
  artifact whose provenance is unrecoverable. `mg[c("a", "b")]` remains the
  cheap escape and has none of the ambiguity.
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
  an edge between two samples.

## References

- `R/spatial_structures.R` — `createSpatialNetwork()`, the `giottoMulti` arm and
  `.csn_child_args()` / `.csn_eval_on_child()`
- `R/gmulti.R` — `.gm_resolve_samples()` (the helper someone reaches for when
  adding a selector), `.gm_set_target()` (why `object =` is exempt)
- `R/classes-space.R` — `combinedSpace` / `perSampleSpace`, the membership split
  this rule depends on
- `vignettes/articles/IMPLEMENTATION_gmulti_federation.md` §14 — the joint
  `@spatial_network` sketch this partially implements
- ADR 0004 — `@network` polymorphism, the other constraint on network writers
