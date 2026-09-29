# =============================================================================
# viewCoordinator — how a view / space recipe is executed on a gobject
# =============================================================================
#
# A coordinator is a dispatch tag: it selects the `resolveKeep()` and
# `resolveRecipe()` leaf methods that evaluate a recipe against the object's
# storage. It does not pick an execution engine -- that belongs to the
# storage itself.
#
# `dataTableCoordinator` is the in-memory one, and the default for an object
# with no `@source`. A backed source selects its own through
# `defaultViewCoordinator()`; GiottoDisk registers `parquetCoordinator` for
# `gsource`. A coordinator that extends another inherits its leaf methods,
# which is how in-memory subobjects inside a backed object are handled.
#
# There is no separate protocol for promoting ID sets or translating
# predicates: each coordinator's methods do that in the form its storage
# wants.
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
