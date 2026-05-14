#' @include classes-virtuals.R
NULL

# ** affine2d ####

#' @title Affine Transform Object
#' @name affine2d-class
#' @aliases affine2d
#' @description
#' Container for accumulating 2D affine transformations. Simple spatial
#' transforms ([spatShift()], [spin()], [rescale()], [flip()], [t()],
#' [shear()]) can be chained on an \code{affine2d} to build up a combined
#' transform, which is then applied to spatial objects in a single step via
#' [affine()].
#'
#' Create an identity \code{affine2d} with \code{affine()} (no arguments).
#' Before chaining centroid-relative operations (\code{spin()},
#' \code{rescale()}) without explicit \code{x0}/\code{y0}, set
#' \code{ext(aff) <- ext(your_object)} so the pivot point matches your data.
#'
#' The combined linear transform is stored as a 3x3 homogeneous matrix in
#' \code{@@affine}. Translations are encoded in column 3 (rows 1-2). The
#' convention is post-multiply: \code{xy_out = xy_in \%*\% A}.
#'
#' @slot anchor numeric(4). Anchoring spatial extent
#'   (\code{xmin, xmax, ymin, ymax}). Set via \code{ext(aff) <-} to match
#'   the data extent. Used as the centroid pivot for [spin()] and [rescale()]
#'   when \code{x0}/\code{y0} are not supplied, and for translation
#'   calculation during transform composition.
#' @slot affine matrix. 3x3 homogeneous transform matrix encoding the
#'   combined linear + translation transform.
#' @slot order character. Records the order in which component operations
#'   were applied.
#' @slot rotate numeric(1). Accumulated rotation angle in radians.
#' @slot shear numeric(2). Accumulated shear factors \code{(x, y)}.
#' @slot scale numeric(2). Accumulated scale factors \code{(x, y)}.
#' @slot translate numeric(2). Accumulated translation \code{(x, y)}.
#' @exportClass affine2d
setClass(
    Class = "affine2d",
    slots = list(
        anchor = "ANY",
        affine = "matrix",
        order = "character",
        rotate = "numeric",
        shear = "numeric",
        scale = "numeric",
        translate = "numeric"
    ),
    prototype = list(
        anchor = c(-180, 180, -90, 90),
        affine = diag(rep(1, 2L)),
        order = c("rotate", "shear", "scale", "translate"),
        rotate = 0,
        shear = c(0, 0),
        scale = c(1, 1),
        translate = c(0, 0)
    )
)

# ** processParam ####

#' @title Parameter Classes for Data Processing Operations
#' @name processParam-class
#' @aliases processParam
#' @description
#' Utility class that defines a data processing procedure and any params used
#' in performing it. Packages defining processing methods will create their own
#' child classes. These parameter objects are intended to be passed alongside
#' the data to process to [processData()].
#' @slot param list. Named parameters to use with the intended processing
#' operation. These can be accessed and updated using the `$` operator.
#' @exportClass processParam
setClass("processParam", contains = "VIRTUAL", slots = list(param = "list"))



# ** svkey ####

#' @name svkey-class
#' @title Spatial Value Key
#' @description
#' A metaprogramming object that references a set of information to get
#' from a `giotto` object when used as `svkey@get(gobject)`.
#' Referenced data will be retrieved as a `data.table` via [spatValues()]
#' @keywords internal
setClass("svkey",
    slots = list(
        feats = "character",
        spat_unit = "nullOrChar",
        feat_type = "nullOrChar",
        expression_values = "nullOrChar",
        spat_loc_name = "nullOrChar",
        spat_enr_name = "nullOrChar",
        poly_info = "nullOrChar",
        dim_reduction_to_use = "nullOrChar",
        dim_reduction_name = "nullOrChar",
        verbose = "nullOrLogical",
        get = "function"
    )
)