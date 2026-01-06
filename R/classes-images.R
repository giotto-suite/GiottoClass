#' @include classes-virtuals.R
#' @include classes-utils.R
NULL

# * definitions ####

#' @title S4 giottoImage Class
#' @description Framework of giotto object to store and work with spatial
#' expression data
#' @concept giotto image object
#' @slot name name of Giotto image
#' @slot mg_object magick image object
#' @slot minmax minimum and maximum of associated spatial location coordinates
#' @slot boundaries x and y coordinate adjustments (default to 0)
#' @slot scale_factor image scaling relative to spatial locations
#' @slot resolution spatial location units covered per pixel
#' @slot file_path file path to the image if given
#' @slot OS_platform Operating System to run Giotto analysis on
#' @details
#' \[\strong{mg_object}\] Core object is any image that can be read by the
#' magick package
#'
#' \[\strong{boundaries}\] Boundary adjustments can be used to manually or
#' automatically through a script adjust the image with the spatial data.
#'
#' @returns giottoImage
#' @examples
#' giottoImage()
#' @export
giottoImage <- setClass(
    Class = "giottoImage",
    slots = c(
        name = "character",
        mg_object = "ANY",
        minmax = "ANY",
        boundaries = "ANY",
        scale_factor = "ANY",
        resolution = "ANY",
        file_path = "ANY",
        OS_platform = "ANY"
    ),
    prototype = list(
        name = "test",
        mg_object = NULL,
        minmax = NULL,
        boundaries = NULL,
        scale_factor = NULL,
        resolution = NULL,
        file_path = NULL,
        OS_platform = NULL
    )
)

#' @title S4 giottoLargeImage Class
#' @description Image class for Giotto that uses \pkg{terra} `SpatRaster` as
#' a backend. If images are loaded from a file on disk then they are worked
#' with lazily, where only the values needed at any moment are loaded/sampled
#' into memory. Since `SpatRaster` objects are C pointers, `giottoLargeImage`
#' and inheriting classes need to run `reconnect()` after loading from a
#' saved object.
#' @concept giotto object image
#' @slot name name of large Giotto image
#' @slot raster_object terra `SpatRaster` object
#' @slot extent tracks the extent of the raster object. Note that most
#' processes should rely on the extent of the raster object instead of this.
#' @slot overall_extent terra extent object covering the original extent of
#' image
#' @slot scale_factor image scaling relative to spatial locations
#' @slot resolution spatial location units covered per pixel
#' @slot max_intensity approximate maximum value
#' @slot min_intensity approximate minimum value
#' @slot max_window value to set as maximum intensity in color scaling
#' @slot colors color mappings in hex codes
#' @slot is_int values are integers
#' @slot file_path file path to the image if given
#' @slot OS_platform Operating System to run Giotto analysis on
#' @returns giottoLargeImage
#' @examples
#' giottoLargeImage()
#'
#' @export giottoLargeImage
#' @exportClass giottoLargeImage
giottoLargeImage <- setClass(
    Class = "giottoLargeImage",
    contains = "giottoSubobject",
    slots = c(
        name = "ANY",
        raster_object = "ANY",
        extent = "ANY", # REMOVE?
        overall_extent = "ANY", # REMOVE? New slot px_dims as replacement?
        scale_factor = "ANY",
        resolution = "ANY",
        max_intensity = "numeric",
        min_intensity = "numeric",
        max_window = "numeric", # NEW
        colors = "character", # NEW
        is_int = "ANY",
        file_path = "ANY",
        OS_platform = "ANY"
    ),
    prototype = list(
        name = NULL,
        raster_object = NULL,
        extent = NULL,
        overall_extent = NULL,
        scale_factor = NULL,
        resolution = NULL,
        max_intensity = NA_real_,
        min_intensity = NA_real_,
        max_window = NA_real_,
        colors = grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1),
        is_int = NULL,
        file_path = NULL,
        OS_platform = NULL
    )
)

#' @title S4 giottoAffineImage Class
#' @description
#' Class extending `giottoLargeImage`. When `shear()` or `spin()` operations
#' are performed on a `giottoLargeImage`, this class is instantiated. It
#' provides a way of storing the affine transformation and also lazily
#' performing it when required for a plotting preview. It is possible to force
#' the deferred affine transform using `doDeferred()` and return a processed
#' `giottoLargeImage`.
#' @slot affine contains `affine2d` object allowing lazily performed spatial
#' transforms
#' @slot funs list of functions associated with the object. Primarily to
#' perform the delayed/lazy operation
#' @returns `giottoAffineImage`
setClass(
    "giottoAffineImage",
    contains = c("giottoLargeImage"),
    slots = c(
        affine = "affine2d",
        funs = "list"
    )
)



# * internals ####

# function for updating image objects if structure definitions have changed
.update_giotto_image <- function(x) {
    if (inherits(x, "giottoLargeImage")) {
        # 0.1.2 release adds colors & max_window slots
        if (is.null(attr(x, "colors"))) {
            attr(x, "colors") <- grDevices::grey.colors(
                n = 256, start = 0,
                end = 1, gamma = 1
            )
        }
        if (is.null(attr(x, "max_window"))) {
            # get a max intensity value
            if (!is.null(x@max_intensity)) {
                x@max_intensity <- .spatraster_intensity_range(
                    x@raster_object
                )[["max"]]
            }

            attr(x, "max_window") <- .bitdepth(
                x@max_intensity,
                return_max = TRUE
            )
        }

        # 0.1.x release adds giottoImageStack
        # deprecate
    }
    return(x)
}