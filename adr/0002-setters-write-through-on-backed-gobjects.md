# 0002. Setters write through to the vault on backed gobjects

- **Status:** Accepted
- **Date:** 2026-05-03
- **Supersedes:** —
- **Superseded by:** —

## Context

Analysis functions across the suite return in-memory results — a `dgCMatrix`
from normalization, an `igraph` from network construction, a `data.table` of
edges — with no knowledge of whether the `giotto` object they came from is
backed by a vault (0001).

If `set*()` were a plain slot assignment, a backed project would accumulate
in-memory content for a whole session and `saveGiotto()` would be the only
moment anything reached disk. Two things break under that arrangement: peak
memory is the whole project rather than one step, and there is no addressable
vault artifact between checkpoints — nothing for a reader, a viewer, or a
resumed session to open. `saveGiotto()` is meant to *be* the checkpoint, not
the write.

## Decision

When `!is.null(gobject@source)` and the incoming payload is not already a
`dataStore`, `set*()` writes it into the vault via `GiottoDisk::sourceWrite()`
and stores the resulting store in the slot:

- `setExpression()` — writes when the matrix is an in-memory `matrix`/`Matrix`,
  records `store@uid` in `x@misc$uid`, and puts `storeRead(store)` back into the
  object. `write = FALSE` forces a write for payloads that are disk-backed by
  some *other* mechanism (e.g. a BPCells `IterableMatrix`), which is otherwise
  passed through untouched.
- `setNearestNetwork()` / `setSpatialNetwork()` — write an `igraph` to a
  `parquetEdgeStore`, plumbing `type =` through so the store's `@type` reflects
  the actual network kind (kNN vs sNN) rather than the `storeWrite` default.
- `setPolygonInfo()` / `setFeatureInfo()` — same shape on `x[]`.

The default is *write only what is in memory*; already-on-disk payloads are
left alone unless the caller opts in.

## Consequences

- `set*()` on a backed gobject performs I/O: it can be slow and it can fail.
  Callers that treated setters as pure slot assignment need to account for that.
- The object in the slot after `set*()` is not the object handed in — it has
  been round-tripped through `sourceWrite()` + `storeRead()`, so its class
  changes. Code that sets and then reads back expecting identity is wrong on
  backed objects.
- Every new setter for a slot that can hold a `dataStore` owes this guard, and
  the guard is currently copy-pasted at five sites rather than factored into a
  helper. That is deliberate while the per-slot write arguments still differ
  (`type =` for networks, `write =` for expression); a
  `.source_write_if_needed()` becomes worthwhile once they converge.
- Revisit when a slot needs write semantics the copy-paste cannot express, or
  when the argument set stabilizes enough to factor.

## Alternatives considered

- **Persist only at `saveGiotto()`** — one bulk write, but it makes peak memory
  the size of the project, produces no intermediate artifacts, and turns a
  checkpoint into the sole point of failure.
- **Require callers to write and pass a store** — pushes vault knowledge into
  every analysis function in Giotto and every third-party method, and makes the
  in-memory and backed call paths textually different.
- **Write through unconditionally, including existing stores** — re-writes
  artifacts that are already in the vault, and duplicates on disk for
  already-disk-backed matrices. This is exactly what `write =` makes opt-in
  instead of default.
- **A separate `setBacked*()` family** — doubles the accessor surface and forces
  every caller to know which world it is in.

## References

- `R/slot_accessors.R` — `setExpression` (`write =` + in-memory matrix test),
  `setNearestNetwork`, `setSpatialNetwork`, `setPolygonInfo`, `setFeatureInfo`
- `R/NN_network.R` — `.finalize_network()`, the construction-time counterpart
  that writes directly when a backend is supplied
- Commits `e59546fb` (forced matrix backed write), `b57c52d9` (network setters
  auto-write), `f2296c12` (`@type` plumbed through), `91b9d039` (tests)
