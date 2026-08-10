# 0003. Five analysis verbs, split by return contract

- **Status:** Accepted
- **Date:** 2026-05-19
- **Supersedes:** —
- **Superseded by:** —

## Context

Analysis steps have to dispatch on how the data is represented — an in-memory
matrix, a GiottoDisk `parquetExprStore`, a BPCells `IterableMatrix` — and the
implementations live in other packages (Giotto, GiottoDisk), so GiottoClass has
to own the generics they attach to.

The design question was how many generics. One surface (`processData(x, param)`
with the `Param` class selecting the operation) is the smallest thing that
dispatches correctly. But the operations do not share a return contract:

| operation | returns |
|---|---|
| normalize, scale | an object of the same shape as `x` |
| filter | a selection — typically `list(feats_keep, cells_keep)` of IDs |
| PCA, UMAP, tSNE | a decomposition — `list(u, d, v, sdev, ...)` |
| label proportions, marker tests | summary statistics, a `data.table` |
| clustering | cluster assignments |

With one generic, the return type is a function of the `Param` argument, so no
consumer can be written against the generic itself.

## Decision

Five distinct generics, all with signature `(x, param, ...)`:

- `processData()` — same-shape transform
- `filterData()` — selection
- `reduceData()` — decomposition / embedding
- `clusterData()` — cluster assignments; `param` takes Bioconductor
  `bluster::BlusterParam` objects
- `analyzeData()` — computed outputs and summary statistics

Each has a matching virtual `Param` class carrying a single `param` list slot
(`processParam`, `filterParam`, `reduceParam`, `analyzeParam`; `clusterData`
borrows bluster's). GiottoClass defines the generics and the virtual classes and
exports **no methods** — every method is attached from a package that knows both
a data representation and an algorithm. `createNetwork()` + `networkParam` are
the `create<Noun>` family's member of the same pattern (0004), deliberately
outside the analysis verbs.

## Consequences

- The return contract is knowable from the generic alone. A caller of
  `reduceData()` never has to test whether it got a selection back.
- A method author must pick the right verb, and picking wrong is quiet rather
  than loud: a method registered on the wrong generic simply never dispatches
  where callers look for it, and tests that route through the wrong verb fall to
  an `ANY,ANY` catch-all instead of failing. Route tests deliberately.
- A new kind of operation means a new generic plus a `Param` pair, not a new
  `type =` string. That is more ceremony per addition and the reason the split
  is five, not fifteen.
- Revisit if two of the five stop having distinguishable return contracts — at
  that point they should merge rather than persist as a distinction only the
  docs can explain.

## Alternatives considered

- **One `processData()` surface, `Param` carries the distinction** — dispatch
  works, but the return type becomes `Param`-dependent and the generic loses its
  contract. Rejected on that alone.
- **A `type =` character argument** — no S4 dispatch, so GiottoDisk cannot
  attach a store-specific implementation without editing GiottoClass. Defeats
  the reason the generics exist.
- **Adopt `bluster`'s `BlusterParam` pattern for all five** — done for
  `clusterData` where the upstream classes already exist; inventing
  bluster-shaped classes for normalization or PCA would tie the suite's
  parameter surface to a clustering framework's conventions.
- **Verbs split by data class instead of by return type** (`processMatrix`,
  `processStore`, ...) — that is what S4 dispatch on `x` already does, and it
  would multiply names by representation rather than by meaning.

## References

- `R/generics.R` — the five `setGeneric()` calls and their return documentation
- `R/classes-utils.R` — `processParam`, `analyzeParam`, `filterParam`,
  `reduceParam`, `networkParam` virtual classes
- Commits `03b6d97b` (`filterData`), `e7f0981f` (`reduceData`), `7990dc76`
  (`calculateLabelProportions` lifted to `analyzeData` with data-class dispatch)
