# 0001. `@source` holds the backend manager; an absent backend is `NULL`

- **Status:** Accepted
- **Date:** 2026-05-17
- **Supersedes:** —
- **Superseded by:** —

## Context

The `giotto` object needed somewhere to record that its data lives in a
GiottoDisk project vault rather than in memory. The existing mechanism was
`@h5_file` — a path, added in the 3.3.1 release, shaped around a single HDF5
file. A vault is a directory of many artifacts governed by a manager object
(`gsource` / `gDirSource`), with behaviour attached to it: where writes land,
how the manifest is updated, how a store is addressed. A path string in a slot
cannot carry any of that.

So `@source` was added (migrated in at `0.5.1` by `.update_source_slot()`) to
hold a `gsource`-inheriting manager, and `createGiottoObject(backend = )` was
made to accept either a path — promoted to `gDirSource(path = )` — or a manager
object directly.

That left one question, which got answered twice: what represents *no backend*.
The first answer was `new("gMemSource")`, a null-object standing for
"in memory", so that write-through sites could call source generics
unconditionally without a guard.

## Decision

`@source` is `NULL` when the object is unmanaged. `.gsource()` returns `NULL`
for such objects; `.gsource<-` rejects anything not inheriting `gsource`. Every
write-through site guards on `!is.null(gobject@source)` (see 0002).

`@h5_file` remains in the class definition, deprecated: `h5_file =` is a
`lifecycle::deprecated()` argument forwarded to `backend` at each entry point
(`createGiottoObject()`, `seuratToGiotto()`, `anndataToGiotto()`), and the slot
is still carried through `[` and `packedGiotto`.

## Consequences

- `is.null(gobject@source)` is the one test for "is this object backed". No
  in-memory object carries vault machinery it will never use.
- The guard is repeated at every write-through site rather than absorbed by a
  null object — five sites in `R/slot_accessors.R` as of this record.
- `@h5_file` is dead weight kept for one release so that deserializing older
  objects does not have to route through a slot-removal migration. It has been
  audited as removable; the removal is owed, and is not blocked on anything in
  this ADR.
- Revisit if a backend variant appears that has no path at all (an in-process
  or remote catalog), since the `gsource` virtual class currently declares
  `path = "character"` and a null object would then be the cheaper encoding.

## Alternatives considered

- **`gMemSource` null object for "no backend"** — tried and reverted. `gsource`
  carries a `path`, which an in-memory object does not have, so the class would
  have existed only to hold `NA_character_`; and it made every plain `giotto`
  object test as backed, pushing the real distinction one level deeper.
- **Store the manager in `@h5_file`** — no new slot, but type-puns a documented
  path slot, and older code reading `@h5_file` as a path would get an S4 object.
- **Backend as an option or `giottoInstructions` entry** — the backend must
  travel with the object through save/load and `join`; instructions are
  session-scoped and would leave `loadGiotto()` guessing where the vault is.

## References

- `R/classes.R` — `source` slot, `.gsource()` / `.gsource<-`,
  `.update_source_slot()`
- `R/create.R` — `backend =` resolution, `h5_file =` deprecation forwarding
- `R/interoperability.R` — same forwarding in the Seurat / AnnData paths
- Commits `3810edb3` (revert of the `gMemSource` null object), `ad59eea5`
  (`h5_file` → `backend` deprecation completed across the create path)
