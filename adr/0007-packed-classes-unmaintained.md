# 0007. `packed*` / `wrap()` / `vect()` are unmaintained; serialize by reference

- **Status:** Accepted
- **Date:** 2026-08-11
- **Supersedes:** —
- **Superseded by:** —

## Context

`giottoPolygon` and `giottoPoints` hold a `terra::SpatVector`, an external
pointer into C++ memory. It does not survive `saveRDS()` or a trip to a worker
process, and it fails *silently* — the object comes back reporting zero
geometries rather than erroring.

The package carries two independent answers to this, and they have drifted
apart in importance:

- **By value.** `terra::wrap()` copies geometry into an R-side representation;
  `packedGiotto`, `packedGiottoPolygon` and `packedGiottoPoints` hold those
  payloads, with `wrap()` / `vect()` converting in both directions. This was the
  original mechanism, aimed at passing objects over a connection to cluster
  nodes.
- **By reference.** `saveGiotto()` writes a directory: each `SpatVector` goes
  out as its own shapefile via `.save_external()` → `terra::writeVector()`, and
  `.load_external()` re-reads them on load. It does not use `wrap()` at all.

Two things settled it. First, by-value serialization requires every geometry to
be resident in the R heap simultaneously, which does not hold for transcript-
scale data — the case the by-value path was nominally for is the case it cannot
serve. Second, the worker-process motivation has an answer that does not involve
shipping geometry at all: attach a `gsource` backend (0001) and let the worker
read from the vault.

## Decision

`wrap()`, `vect()` on `packed*` classes, and the `packed*` classes themselves
are **unmaintained and may be removed in a future release. New code must not be
built on them.**

- New terra-backed classes get `.save_external()` / `.load_external()` methods
  and **no** `packed*` variant.
- Persistence goes through `saveGiotto()` / `loadGiotto()`.
- Reaching data from a worker process goes through a `gsource` backend, not
  through shipping geometry by value.
- The existing methods stay in place and keep working. They are not removed
  yet because `{GiottoData}`'s `giottoPoints` and `giottoPolygon` minis are
  stored on disk as `packedGiotto*` (`GiottoData/R/mini_subobjects.R:22` calls
  `GiottoClass::vect()` on them), and users have older `.RDS` files of wrapped
  objects. Note `loadGiottoMini()` does *not* come through this path; it uses
  `loadGiotto()`.

**No runtime deprecation warning is emitted.** This was tried and reverted. The
read direction (`vect()`) is exactly the path that legitimately still runs, so
warning there penalizes users for data they did not create. `deprecate_soft()`
looked like the answer — silent for indirect calls, audible for direct ones —
but it did not separate the two cases here in practice. Warning only the write
direction (`wrap()`) was also implemented and reverted as more machinery than
the situation warrants while removal is not scheduled. The signal is carried by
the roxygen block on `?wrap`, a comment at the top of `R/methods-wrap.R`,
`AGENTS.md`, and this record.

## Consequences

- Anyone adding a terra-backed subobject now has one serialization obligation
  instead of two. The `packed*` boilerplate is not owed.
- A saved Giotto object is a *directory*, not a file. Moving a project means
  moving all of it. This was already true; the decision commits to it.
- The suite cannot drop `wrap()` / `vect()` until `{GiottoData}` re-ships its
  packed minis by reference. That is the concrete unblock condition for
  removal, and it lives in another repository.
- One property of the current design is worth preserving in whatever replaces
  it: `packedGiotto` deliberately does **not** inherit from `giotto`, so a
  packed object fails dispatch rather than silently satisfying a method that
  expects live pointers.
- No user-visible behaviour changes with this record. It is a statement of
  direction, so nothing is owed in `NEWS.md` until removal.

## References

- `R/methods-wrap.R` — the methods and the code pointer to this record.
- `R/save_load.R` — `.save_external()` / `.load_external()`, the by-reference
  path.
- `vignettes/articles/design.Rmd`, *terra-backed classes and the pointer
  problem*.
- ADR 0001 — `@source` holds the backend manager, which is what makes the
  worker-process case answerable by reference.
