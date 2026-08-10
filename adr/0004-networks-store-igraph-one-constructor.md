# 0004. Networks store a graph in `@network`; `createNetwork()` is the one constructor

- **Status:** Accepted
- **Date:** 2026-05-21
- **Supersedes:** —
- **Superseded by:** —

## Context

Two network classes stored the same kind of thing two different ways.
`spatialNetworkObj` held a `data.table` of edges *carrying coordinate columns*
in `@networkDT`, plus a second pre-filter copy in `@networkDT_before_filter`.
`nnNetObj` held an `igraph` in `@igraph`.

The `data.table` form needed `convert_to_full_spatial_network()` and
`convert_to_reduced_spatial_network()` to emulate undirected semantics that a
graph has natively, and it duplicated coordinates that already live in
`spatLocsObj`.

Construction was spread across `createSpatialNetwork()`,
`createSpatialKNNnetwork()`, `createSpatialDelaunayNetwork()` and
`createNearestNetwork()` — four argument surfaces, each resolving its own
defaults, and none able to emit a disk-backed edge store for a vault-managed
project (0002).

## Decision

Both classes store the graph in a slot named `network`: `spatNetData` gets
`network` + `unfiltered`, `nnData` gets `network`. The content is an `igraph`
(undirected for spatial networks, directed for kNN) — or a GiottoDisk
`dataStore`.

Construction goes through one generic, `createNetwork(x, param)`, dispatching on
data class × `networkParam` subclass (`kNNNetworkParam`, `sNNNetworkParam`,
`delaunayNetworkParam`), with methods for `matrix`, `spatLocsObj`, `dimObj` and
`giotto`. The Param's `output` slot picks `"data.table"` / `"igraph"` /
`"parquet"`, and `"auto"` resolves to parquet when a `backend` is supplied. The
four legacy constructors become thin wrappers that build a Param and call it.

Pre-`0.6.0` serialized objects migrate on load: `.update_network_slots()` reads
legacy content from `attr()` under the old slot names — R's S4 deserialization
keeps the stored representation when a class definition has changed — and
rebuilds via the constructors, converting edge tables to `igraph` on the way.

## Consequences

- Coordinates are no longer duplicated inside the network, and both-directions
  semantics come from igraph rather than from an expand/filter/reduce cycle.
  `convert_to_full_spatial_network()` and its reduced counterpart are gone;
  consumers that genuinely need a full edge table build it with one `rbind`.
- **`@network` is polymorphic** (`igraph` | `dataStore`). Anything that reaches
  into the slot and calls an igraph function on it breaks on a backed project.
  This is a live gap, not a solved problem: `spatIDs()` on `nnNetObj` /
  `spatialNetworkObj` delegates when the slot holds a `dataStore`, and
  `.evaluate_*_network()` accepts one, but there is no general node-id accessor —
  a `nodeIDs()` generic is the intended fix. Until then, every new consumer of
  `@network` owes a branch.
- The migration path must stay for as long as pre-`0.6.0` objects circulate, and
  it is load-bearing for `GiottoData` mini objects.
- Revisit the polymorphic slot if the branch count grows past what a `nodeIDs()`
  generic plus the existing delegations can absorb.

## Alternatives considered

- **Keep the `data.table` representation, convert to igraph at use sites** —
  keeps the coordinate duplication and the full/reduced helpers, and leaves two
  classes with two representations of one concept.
- **Store both igraph and edge table** — doubles memory for large networks and
  invites the two copies to drift.
- **Make the edge table canonical and store edges only** — every consumer still
  needs a graph library, and `igraph` is already a hard dependency, so the
  conversion would be paid repeatedly for no saving.
- **A separate class for disk-backed networks** instead of a polymorphic slot —
  doubles every network method and splits the accessor surface, which is a
  larger tax than the branches the polymorphic slot costs.
- **Keep the four constructors and add a fifth for stores** — the argument
  surfaces were already inconsistent; a fifth would not have been able to share
  the filter/weight logic.

## References

- `R/classes-virtuals.R` — `nnData`, `spatNetData` slot definitions
- `R/classes.R` — `.update_network_slots()`, `.migrate_spatnet_obj()`,
  `.migrate_nn_net_obj()`, `.legacy_spatnet_dt_to_igraph()`
- `R/NN_network.R` — `networkParam` subclasses, `createNetwork()` methods,
  `.finalize_network()` output resolution, `createNearestNetwork()` wrapper
- `R/data_evaluation.R`, `R/methods-IDs.R` — the `dataStore` branches
- Commits `d9128699`, `d9ca3126`, `3d462324`, `139b5f09`, `c9210779`,
  `b1579fe1`, `411105d4`, `b343cfd4`
