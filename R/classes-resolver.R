# =============================================================================
# viewCoordinator — cross-storage bridging for view + space resolution
# =============================================================================
#
# A `viewCoordinator` is the dispatch tag for HOW a giottoView + giottoSpace
# recipe gets executed across the gobject's storage backings. It is NOT a
# strategy class for picking an execution engine — the engine is owned by
# the storage (GiottoDisk parquet stores expose lazy ops + storeRead output
# modes that already select the engine at read time).
#
# Pattern mirrors GiottoClass's other strategy generics (`processData`,
# `analyzeData`, etc.) where the entry-point generic dispatches on both the
# data class and the strategy class. Here the entry point is
# `resolve(subobj, coordinator, keep =, space =, view =)`, and it is the ONLY
# entry point.
#
# There is no separate coordinator protocol. The original sketch (2026-05-28)
# proposed three generics -- `prepareIds()` to promote an ID set into the
# backend's form, `applyIdsFilter()` to apply it, `translatePredicate()` to
# map an R predicate into the backend's filter language. They were designed
# before the leaf generic dispatched on the coordinator; once it does, they
# select on exactly the same thing one layer further down. Each coordinator's
# `resolve` methods do all three internally, in the form its storage wants,
# and share the work between their own leaves with ordinary internal helpers
# rather than exported generics. `prepareIds()` shipped as an exported
# identity transform with zero call sites and was removed in 0.7.2; the other
# two were never written.
#
# Concrete coordinators:
#   dataTableCoordinator   — all in-memory; IDs as R character vector;
#                            apply via [cell_ID %in% ids]. Lives in
#                            GiottoClass. Default for non-disk gobjects;
#                            also the always-works in-memory fallback when
#                            mixed gobjects need promotion.
#   duckDBCoordinator      — ID promotion via duckdb_register_arrow /
#                            ephemeral table; apply via JOIN. Lives in
#                            GiottoDisk; registered when loaded.
#   sedonaCoordinator      — sedona view registration; full spatial
#                            vocabulary at apply time. Lives in GiottoDisk.
#
# Default selection from `gobject@source`: no source → dataTableCoordinator;
# parquet/sedona/duckdb-backed sources → respective concrete coordinator
# via the S3 hook `defaultViewCoordinator.<source_class>` registered from
# GiottoDisk. See `.default_view_coordinator()` in methods-resolver.R.
# =============================================================================


#' @title viewCoordinator virtual class
#' @description Base class for view + space resolution coordinators —
#' polymorphic dispatch surface that brokers IDs and joins between storage
#' backings during view resolution. The execution engine itself is owned by
#' the storage (see [GiottoDisk::storeRead] and its `output` modes).
#'
#' Concrete subclasses include [dataTableCoordinator-class] in GiottoClass and
#' (registered when loaded) `duckDBCoordinator` / `sedonaCoordinator` in
#' GiottoDisk.
#' @slot misc `list` for backend-specific options.
#' @returns `viewCoordinator`-inheriting object
#' @export
#' @exportClass viewCoordinator
setClass(
    "viewCoordinator",
    contains = "VIRTUAL",
    slots = list(misc = "list"),
    prototype = list(misc = list())
)


#' @title dataTableCoordinator
#' @description In-memory coordinator. Carries surviving cell_IDs as a plain
#' R character vector and applies them via `[cell_ID %in% ids]`-style
#' filtering. The reference implementation and always-works fallback for
#' any gobject regardless of backing (at the cost of materializing backed
#' subobjects into R memory).
#'
#' @returns `dataTableCoordinator`
#' @examples
#' dataTableCoordinator()
#' @export
#' @exportClass dataTableCoordinator
setClass("dataTableCoordinator", contains = "viewCoordinator")

#' @rdname dataTableCoordinator-class
#' @export
dataTableCoordinator <- function() new("dataTableCoordinator")
