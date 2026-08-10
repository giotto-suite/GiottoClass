# Architecture Decision Records

One file per architectural decision, dated and immutable. An ADR records *why a
choice was made at a point in time*, including what was rejected and what the
choice costs. It is history, not documentation of current behaviour.

This directory follows the same conventions as GiottoDisk's `adr/`, so a
decision that spans both packages reads the same way in either repo.

## Why these exist alongside the other docs

Keep the boundary sharp — each of these answers a question the others do not:

| Doc | Answers | Tense |
|---|---|---|
| `CLAUDE.md` | What invariants hold right now, and where the code is | present, terse |
| `vignettes/overview.Rmd` | How the object model works and hangs together | present, narrative |
| `NEWS.md` | What changed, per release | past, per-version |
| `adr/` (here) | Why we chose this over the alternatives, and when | past, immutable |

Practical test for where something belongs:

- "Generics go in `R/generics.R`, methods in `R/methods-<topic>.R`" → CLAUDE.md
  (a convention a code change must respect).
- "`giotto` slot lists are keyed by `spat_unit` / `feat_type`" → overview.Rmd.
- "`spatialNetworkObj` slots were renamed in 0.6.0" → NEWS.md.
- "We rejected a `gMemSource` null object because an absent backend has no path"
  → ADR.

The overlap is intentional and one-directional: CLAUDE.md and the vignettes
state the *outcome* of an ADR without rehearsing the argument; the ADR is where
the argument and the discarded options live. When they disagree, CLAUDE.md wins
for current behaviour and the ADR wins for intent — and the disagreement is
itself a signal that a superseding ADR is owed.

## Branch note

This directory was started on `gsource`, the long-running integration branch
where the disk-backed arc is staged before it flows into `dev`. The records here
are not gsource-only history — they describe decisions the merged package
carries, and they travel with the merge. An ADR is still owed for a purely
in-memory decision made on `dev`; number it there and let the branches
reconcile the index.

## Writing one

1. Copy `0000-template.md` to `NNNN-short-kebab-title.md`, taking the next free
   number. Numbers are record order, not decision order — an ADR backfilled
   today for a 2025 decision still takes the next number and carries the older
   date.
2. Fill it in. Keep it to a page; if it needs more, the extra belongs in a
   vignette and the ADR should link to it.
3. Add a row to the index below.

## Amending one

An accepted ADR is not edited, with two exceptions: its `Status` line, and links
added to it (`Superseded by`). Everything else changes by writing a new ADR that
supersedes it. A wrong ADR that got reversed is more useful than no record of
the reversal.

Statuses: **Proposed** · **Accepted** · **Superseded by NNNN** · **Reversed**
(tried, undone, nothing replaced it) · **Deprecated** (still true, no longer
load-bearing).

## Scope — what earns an ADR

Something an outsider (or you in six months) would otherwise change by accident.
Roughly: a decision that constrains future code, was contested or non-obvious,
or has a cost worth remembering.

Not: bug fixes, refactors that preserve behaviour, or naming conventions (those
go in CLAUDE.md "Coding Conventions").

## Finding these

The index below is the fallback. The primary path is a pointer **from the code
the decision constrains** — `adr/NNNN` in a comment at the site someone would
edit. That is where an ADR gets read: at the moment you are about to change the
thing, not while browsing.

So when you add one, add the pointer too, and put it where the tempting edit
would be made rather than where the topic is documented.

**None of the records below carry code pointers yet** — they were written as a
batch ahead of the pointer pass. Adding them is owed; the sites are named in
each record's *References*.

## Index

| # | Title | Status | Date |
|---|---|---|---|
| [0001](0001-source-slot-holds-the-backend.md) | `@source` holds the backend manager; an absent backend is `NULL` | Accepted | 2026-05-17 |
| [0002](0002-setters-write-through-on-backed-gobjects.md) | Setters write through to the vault on backed gobjects | Accepted | 2026-05-03 |
| [0003](0003-five-verb-generics.md) | Five analysis verbs, split by return contract | Accepted | 2026-05-19 |
| [0004](0004-networks-store-igraph-one-constructor.md) | Networks store a graph in `@network`; `createNetwork()` is the one constructor | Accepted | 2026-05-21 |

## Backfill candidates

Decisions already argued out elsewhere in the repo or in commit messages, not
yet written up. Not a queue — write one when it next comes up in conversation,
so the ADR captures the argument while it is fresh.

- **Validators pass store classes through unchanged.**
  `.evaluate_expr_matrix()` lets a `parquetExprStore` through rather than
  coercing it, on the principle that a validator must never materialize its
  input. Constrains every `.evaluate_*` function. (`d08eed64`)
- **`nodeIDs()` generic for polymorphic `@network`.** The gap named in 0004:
  `spatIDs()` and `.evaluate_*_network()` each got their own `dataStore` branch,
  and the general accessor that would retire those branches has not been chosen
  yet. (`411105d4`, `b343cfd4`)
- **`spatRelate()` generic lives in GiottoClass, engines live in GiottoDisk.**
  Why the generic and the `giottoSpatial` method sit here while the
  sedona/duckdb/terra dispatch sits downstream. (`0c8588b5`, `b1e74a13`)
- **`saveGiotto()` / `loadGiotto()` change meaning under a `gsource`.**
  `foldername` and related arguments are ignored when the project is managed,
  because the vault owns layout. Constrains anything that computes a save path.
- **Setter `verbose` defaults are `NULL`, not `TRUE`.** So `vmsg()` can resolve
  verbosity from instructions rather than having it pinned at the call site.
  (`8e056970`, `5f0140ba`)
- **Accessor generic formals widening + `name = NULL` setter default.** Widening
  a slot-accessor generic forces the method default to `NULL`, because S4
  propagates the generic's default symbolically and breaks
  `is.null(match.call()$name)` detection. Argued out on
  `feature/accessor-generic-formals` (PR #381, on hold) — not on this branch.
- **`@h5_file` removal.** Audited as removable, deliberately kept deprecated for
  one release (0001). The removal itself deserves a line in NEWS.md rather than
  an ADR, unless the deserialization path turns out to need a migration.
