#' @include classes-virtuals.R
NULL

# ** affine2d ####
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