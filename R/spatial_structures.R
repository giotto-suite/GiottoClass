## Spatial structure helper functions ####








#' @title get_distance
#' @name get_distance
#' @description estimate average distance between neighboring cells with network
#' table as input
#' @param networkDT networkDT
#' @param method method
#' @returns numeric
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' spat_net <- getSpatialNetwork(g, output = "networkDT")
#'
#' get_distance(spat_net, method = "mean")
#' @export
get_distance <- function(networkDT,
    method = c("mean", "median")) {
    distance <- switch(method,
        "median" = stats::median(networkDT$distance),
        "mean" = mean(networkDT$distance)
    )
    return(distance)
}





#' @title Filter spatial network
#' @name .filter_network
#' @description Filter a spatial network by spatial characteristics
#' @param networkDT spatial network in data.table format
#' @param maximum_distance maximum distance between cell centroids
#' @param minimum_k minimum number of neighbors
#' @keywords internal
#' @returns data.table
.filter_network <- function(networkDT = NULL,
    maximum_distance = NULL,
    minimum_k = 0L) {
    # data.table variables
    distance <- rank_from <- rank_to <- from <- to <- NULL

    if (is.null(maximum_distance)) return(networkDT)

    dt <- data.table::copy(networkDT)

    # per-side distance rank — lets the `minimum_k` floor apply
    # symmetrically (each node retains at least k neighbours) without
    # expanding the canonical (from < to) representation.
    data.table::setorder(dt, from, distance)
    dt[, rank_from := seq_len(.N), by = "from"]
    data.table::setorder(dt, to, distance)
    dt[, rank_to := seq_len(.N), by = "to"]

    cutoff <- if (isTRUE(maximum_distance == "auto")) {
        grDevices::boxplot.stats(dt$distance)$stats[5]
    } else maximum_distance

    dt <- dt[distance <= cutoff | rank_from <= minimum_k | rank_to <= minimum_k]
    dt[, c("rank_from", "rank_to") := NULL][]
}







#' @title Compatible spatial network
#' @name compatible_spatial_network
#' @description Function to evaluate if a spatial network is compatible
#' with a provided expression matrix
#' @param spatial_network spatial network to evaluate
#' @param expression_matrix expression to compare against
#' @returns TRUE or character
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' spat_net <- getSpatialNetwork(g, output = "networkDT")
#' expr_m <- getExpression(g)
#'
#' compatible_spatial_network(spat_net, expr_m)
#' @export
compatible_spatial_network <- function(spatial_network,
    expression_matrix) {
    # first evaluate spatial network
    spatial_network <- .evaluate_spatial_network(spatial_network)

    # compatible network
    # all network nodes need to be found back in the column names

    network_ids <- unique(spatial_network$from, spatial_network$to)
    cell_ids <- colnames(expression_matrix)

    missing_network_ids <- network_ids[!network_ids %in% cell_ids]

    if (length(missing_network_ids) > 0) {
        stop(
            "Spatial network ids missing in expression matrix: ",
            list(missing_network_ids)
        )
    } else {
        return(TRUE)
    }
}













#' @title Create a spatial Delaunay network
#' @name createSpatialDelaunayNetwork
#' @description Create a spatial Delaunay network based on cell centroid
#' physical distances.
#' @param gobject giotto object
#' @param name name for spatial network (default = 'delaunay_network')
#' @param default_name name to fall back on when `name` is `NULL`, before the
#'   coordinate frame is prefixed. Exists so that one composition site can
#'   serve every entry point while each keeps its own spelling; callers do not
#'   normally set it.
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations
#' @param method package to use to create a Delaunay network
#' @param spat_loc_name name of spatial locations
#' @param dimensions which spatial dimensions to use. Use
#' "sdimx" (spatial dimension x), "sdimy", "sdimz" respectively to refer to
#' X (or the 1st), Y (or the 2nd) and Z(or the 3rd) dimension, see
#' details. (default = all)
#' @param maximum_distance distance cuttof for Delaunay neighbors to consider.
#' If "auto", "upper whisker" value of the distance vector between neighbors
#' is used; see the [graphics::boxplot()] documentation for more
#' details.(default = "auto")
#' @param minimum_k minimum number of neighbours if maximum_distance != NULL
#' @param options (geometry) String containing extra control options for the
#' underlying Qhull command; see the
#' [Qhull documentation](http://www.qhull.org/html/qdelaun.htm) for the
#' available options. (default = 'Pp', do not report precision problems)
#' @param Y (RTriangle) If TRUE prohibits the insertion of Steiner points on
#' the mesh boundary.
#' @param j (RTriangle) If TRUE jettisons vertices that are not part of the
#' final triangulation from the output.
#' @param S (RTriangle) Specifies the maximum number of added Steiner points.
#' @inheritParams createSpatialNetwork
#' @param \dots Other additional parameters
#' @returns giotto object with updated spatial network slot
#' @details Creates a spatial Delaunay network as explained
#' in \code{\link[geometry]{delaunayn}} (default), \code{\link[deldir]{deldir}},
#' or \code{\link[RTriangle]{triangulate}}.
#' @section Choosing a Delaunay backend:
#' All three backends compute the same exact triangulation -- on 50,000 uniform
#' points each returns the identical 149,978 edges -- so the choice is purely
#' one of implementation speed, and `deldir` is by a wide margin the slowest:
#'
#' | points | `deldir` | `delaunayn_geometry` |
#' |---|---|---|
#' | 20,000 | 3.7 s | 0.14 s |
#' | 50,000 | 21.1 s | 0.20 s |
#' | 200,000 | ~363 s | 0.94 s |
#'
#' `deldir` remains the default for backward compatibility, but
#' **`delaunay_method = "delaunayn_geometry"` is strongly preferred above a few
#' thousand points**. It requires the \pkg{geometry} package, and it is also the
#' only backend that handles 3D.
#'
#' Two caveats. Qhull can struggle with exactly cocircular input -- a
#' grid-aligned platform such as Visium -- in which case pass
#' `options = "Qbb Qc Qz"`; a 10,000-point exact grid worked with the default
#' `"Pp"` in testing, but the failure mode is degenerate input rather than
#' size. And `deldir` peaks around 2.9 GB of memory at 200,000 points against
#' `geometry`'s 0.24 GB, so on a large section it is the memory, not just the
#' wait, that bites.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialDelaunayNetwork(g)
#' @export
createSpatialDelaunayNetwork <- function(gobject,
    name = NULL,
    default_name = "Delaunay_network",
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    method = c("deldir", "delaunayn_geometry", "RTriangle"),
    dimensions = "all",
    maximum_distance = "auto",
    minimum_k = 0,
    options = "Pp",
    Y = TRUE, j = TRUE, S = 0,
    verbose = TRUE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    space = NULL,
    ...) {
    # Thin wrapper over createNetwork() + spatialNetworkObj construction.
    method <- match.arg(method, c("deldir", "delaunayn_geometry", "RTriangle"))

    # `method` is kept in the wrapper's spelling for @method and the param
    # record; the Param API calls the same backend "geometry".
    param_method <- if (method == "delaunayn_geometry") "geometry" else method

    .create_spatial_network_from_param(
        gobject = gobject,
        param = delaunayNetworkParam(
            method = param_method,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            output = "igraph",
            options = options, Y = Y, j = j, S = S
        ),
        method = method,
        parameters = list(
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            dimensions = dimensions
        ),
        spat_unit = spat_unit,
        spat_loc_name = spat_loc_name,
        dimensions = dimensions,
        name = name,
        default_name = default_name,
        verbose = verbose,
        return_gobject = return_gobject,
        output = output,
        space = space,
        ...
    )
}












#' @title createSpatialKNNnetwork
#' @name createSpatialKNNnetwork
#' @description Create a spatial knn network.
#' @param gobject giotto object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param name name for spatial network (default = 'spatial_network')
#' @param default_name name to fall back on when `name` is `NULL`, before the
#'   coordinate frame is prefixed. Exists so that one composition site can
#'   serve every entry point while each keeps its own spelling; callers do not
#'   normally set it.
#' @param method method to create kNN network
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations
#' @param dimensions which spatial dimensions to use (default = all)
#' @param k number of nearest neighbors based on physical distance
#' @param maximum_distance distance cuttof for nearest neighbors to consider
#' for kNN network
#' @param minimum_k minimum nearest neigbhours if maximum_distance != NULL
#' @param verbose verbose
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @inheritParams createSpatialNetwork
#' @param \dots additional arguments to the selected method function
#' @returns giotto object with updated spatial network slot
#'
#' \strong{dimensions: } default = 'all' which takes all possible dimensions.
#' Alternatively you can provide a character vector that specififies the
#' spatial dimensions to use, e.g. c("sdimx', "sdimy")
#' or a numerical vector, e.g. 2:3
#'
#' \strong{maximum_distance: } this is a post-filter on the k neighbours the
#' search already found, not a constraint on the search itself. To build a
#' network on distance alone, prefer [radiusNetworkParam()], which searches
#' only within the radius; the older advice of setting `k` very high (e.g.
#' `k = 100`) and filtering still works, but it finds a hundred neighbours per
#' cell in order to discard most of them.
#'
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialKNNnetwork(g)
#'
#' @export
createSpatialKNNnetwork <- function(gobject,
    method = "dbscan",
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    name = NULL,
    default_name = "knn_network",
    k = 4,
    maximum_distance = NULL,
    minimum_k = 0,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    space = NULL,
    ...) {
    # Thin wrapper over createNetwork() + spatialNetworkObj construction.
    method <- match.arg(method, c("dbscan"))

    .create_spatial_network_from_param(
        gobject = gobject,
        param = kNNNetworkParam(
            k = k,
            filter = TRUE,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            output = "igraph",
            # Spatial coordinates are 2-3 dimensional, where an exact kd-tree
            # search is optimal and an HNSW index never amortizes its build.
            # This is kNNNetworkParam()'s own default; stated here so a future
            # flip of that default cannot silently reroute spatial networks
            # through the approximate path.
            engine = "dbscan"
        ),
        method = method,
        parameters = list(
            k = k,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            dimensions = dimensions
        ),
        spat_unit = spat_unit,
        spat_loc_name = spat_loc_name,
        dimensions = dimensions,
        name = name,
        default_name = default_name,
        verbose = verbose,
        return_gobject = return_gobject,
        output = output,
        space = space,
        ...
    )
}









## spatial network ####

#' @title Create spatial centroid connectivity network
#' @name createSpatialNetwork
#' @description Create a spatial network based on cell centroids. These networks
#' are often used when determining cell-cell connectivities and spatial
#' relationships.
#' There are several types of spatial networks and multiple methods to generate
#' them. Method-specific params are labeled with the name of the method within
#' parentheses in their descriptions.
#' @param gobject giotto object
#' @param name name for spatial network (default = 'spatial_network')
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations to use
#' @param dimensions which spatial dimensions to use (default = all)
#' @param method which method to use to create a spatial network. One of
#' `"Delaunay"` (default), `"kNN"`, or `"radius"`. `"radius"` connects every
#' pair of cells closer together than `radius`, so unlike kNN it gives dense
#' regions more neighbours than sparse ones.
#' @param delaunay_method method to use to generate Delaunay network. All
#' three give the identical triangulation; `"delaunayn_geometry"` is far faster
#' above a few thousand points (0.94 s vs ~363 s at 200,000) and is the only
#' one that handles 3D. `"deldir"` remains the default for backward
#' compatibility. See the *Choosing a Delaunay backend* section of
#' [createSpatialDelaunayNetwork()].
#' @param maximum_distance_delaunay distance cutoff for nearest neighbors to
#' consider for Delaunay network. If "auto", "upper whisker" value of the
#' distance vector between neighbors is used; see the [grDevices::boxplot.stats]
#' documentation for more details.(default = "auto")
#' @param options (geometry) String containing extra control options for the
#' underlying Qhull command; see the
#' [Qhull documentation](http://www.qhull.org/html/qdelaun.htm) for the
#' available options. (default = 'Pp', do not report precision problems)
#' @param Y (RTriangle) If TRUE prohibits the insertion of Steiner points on
#' the mesh boundary.
#' @param j (RTriangle) If TRUE jettisons vertices that are not part of the
#' final triangulation from the output.
#' @param S (RTriangle) Specifies the maximum number of added Steiner points.
#' @param knn_method method to create kNN network
#' @param k number of nearest neighbors based on physical distance
#' @param minimum_k minimum nearest neighbours if maximum_distance != NULL
#' @param maximum_distance_knn distance cutoff for nearest neighbors to consider
#' for kNN network
#' @param radius (radius) distance cutoff, in the units of the spatial
#' locations. Every pair of cells within this distance of each other is
#' connected. Required when `method = "radius"`.
#' @param verbose be verbose
#' @param return_gobject logical. return giotto object (default = TRUE)
#' @param output character. Object type to return spatial network as when
#' `return_gobject = FALSE`. (default: 'spatialNetworkObj')
#' @param space (`giottoMulti` only) `character(1)`. Name of a coordinate
#' frame recorded on the object, or `NULL` (default) for each sample's own
#' native frame. The frame decides the job: every sample it covers gets a
#' network built in that frame's coordinates, written into its own
#' `@spatial_network` slot — so this mutates the wrapped children.
#'
#' This is **not** a sample selector. An artifact generator takes none,
#' because once the rows are in a slot nothing downstream can tell which
#' the selector admitted (see `adr/0006`). To build over a subset of
#' samples, record a space over them, or subset with `mg[...]` first.
#' @param \dots Additional parameters for the selected function
#' @returns giotto object with updated spatial network slot
#' @details Creates a spatial network connecting single-cells based on their
#' physical distance to each other.
#' For Delaunay method, neighbors will be decided by Delaunay triangulation and
#' a maximum distance criteria. For kNN method, number of neighbors can be
#' determined by k, or maximum distance from each cell with or without
#' setting a minimum k for each cell.
#'
#' **dimensions: ** default = 'all' which takes all possible dimensions.
#' Alternatively you can provide a character vector that specifies the spatial
#' dimensions to use, e.g. c("sdimx', "sdimy") or a numerical vector, e.g. 2:3
#'
#' @md
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialNetwork(g)
#' @export
createSpatialNetwork <- function(gobject,
    name = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    method = c("Delaunay", "kNN", "radius"),
    delaunay_method = c("deldir", "delaunayn_geometry", "RTriangle"),
    maximum_distance_delaunay = "auto",
    options = "Pp",
    Y = TRUE,
    j = TRUE,
    S = 0,
    minimum_k = 0,
    knn_method = "dbscan",
    k = 4,
    maximum_distance_knn = NULL,
    radius = NULL,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    space = NULL,
    ...) {
    # `space` is a COORDINATE FRAME name, not a set of samples: an artifact
    # generator takes no sample selector, and its job size is read from the
    # frame. See adr/0006. A giottoMulti needs no dispatch here -- all three
    # arms below reach `.create_spatial_network_from_param()`, which reads
    # the job size off the space for every door at once.
    method <- match.arg(method, c("Delaunay", "kNN", "radius"))

    if (method == "kNN") {
        knn_method <- match.arg(knn_method, c("dbscan"))

        out <- createSpatialKNNnetwork(
            gobject = gobject,
            # keep this door's own spelling; the wrapper's differs
            default_name = "kNN_network",
            spat_unit = spat_unit,
            feat_type = feat_type,
            method = knn_method,
            spat_loc_name = spat_loc_name,
            dimensions = dimensions,
            k = k,
            maximum_distance = maximum_distance_knn,
            minimum_k = minimum_k,
            name = name,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            space = space,
            ...
        )
    } else if (method == "Delaunay") {
        delaunay_method <- match.arg(
            delaunay_method,
            c("deldir", "delaunayn_geometry", "RTriangle")
        )
        out <- createSpatialDelaunayNetwork(
            gobject = gobject,
            # keep this door's own spelling; the wrapper's differs
            default_name = "Delaunay_network",
            spat_unit = spat_unit,
            feat_type = feat_type,
            spat_loc_name = spat_loc_name,
            method = delaunay_method,
            dimensions = dimensions,
            name = name,
            maximum_distance = maximum_distance_delaunay,
            options = options,
            minimum_k = minimum_k,
            Y = Y,
            j = j,
            S = S,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            space = space,
            ...
        )
    } else if (method == "radius") {
        if (is.null(radius)) {
            stop(wrap_txt(
                'method = "radius" needs a `radius` (a distance in the units',
                "of the spatial locations)."
            ), call. = FALSE)
        }
        out <- .create_spatial_network_from_param(
            gobject = gobject,
            param = radiusNetworkParam(
                eps = radius, minimum_k = minimum_k, output = "igraph"
            ),
            method = "radius",
            parameters = list(
                eps = radius, minimum_k = minimum_k, dimensions = dimensions
            ),
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            dimensions = dimensions,
            name = name,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            space = space,
            ...
        )
    }

    return(out)
}


# giottoMulti network builds ####
#
# The space says how big the job is (adr/0006), and that is the whole of the
# decision:
#
#   combinedSpace  -- its samples share one coordinate system, so a job over
#                     it is ONE job. Locations are fused across its members
#                     into a single table of `sample::id` globals, one
#                     network is built spanning them, and it goes in the
#                     multi's own @spatial_network. This is the only place a
#                     cross-sample edge can exist, and the reason that slot
#                     exists at all.
#   perSampleSpace -- each sample gets its own copy of the frame, so a job
#                     over it is N independent jobs, one written per child.
#
# Reached from `.create_spatial_network_from_param()`, which means all three
# public doors get it. Previously only `createSpatialNetwork()` handled a
# multi at all and it planned every frame per-sample, so a `combinedSpace`
# built N networks in a shared frame and missed exactly the cross-sample
# edges it exists for; the two wrappers died on `incorrect number of
# dimensions` because `getSpatialLocations(mg)` hands back a list.
#
# `param` arrives already built, which is also what retires the call-replay
# this used to do. That machinery existed to avoid hand-listing 19 formals
# to forward per child -- a list that went stale when `radius` arrived
# upstream, binding at the container and reaching no child. A built `param`
# has no formals to forget, so there is nothing left to drift. Do not
# reintroduce a forward. See adr/0006.

#' @noRd
.csn_multi <- function(gobject, param, method, parameters, space,
    spat_unit, spat_loc_name, dimensions, name, verbose,
    return_gobject, output, ...) {
    if (length(gobject@objects) == 0L) {
        stop("[createSpatialNetwork] giottoMulti has no child gobjects",
            call. = FALSE)
    }

    if (inherits(space, "combinedSpace")) {
        return(.csn_combined(gobject, param = param, method = method,
            parameters = parameters, space = space, spat_unit = spat_unit,
            spat_loc_name = spat_loc_name, dimensions = dimensions,
            name = name, verbose = verbose,
            return_gobject = return_gobject, output = output, ...))
    }

    # per-sample: N artifacts, and no single one to hand back
    if (!isTRUE(return_gobject)) {
        stop("[createSpatialNetwork] a per-sample job builds one network ",
            "per child, so it needs the container to write them into: ",
            "`return_gobject = TRUE`. A combinedSpace builds one network ",
            "and can return it.", call. = FALSE)
    }
    for (nm in names(gobject@objects)) {
        gobject@objects[[nm]] <- .create_spatial_network_from_param(
            gobject = gobject@objects[[nm]],
            param = param, method = method, parameters = parameters,
            spat_unit = spat_unit, spat_loc_name = spat_loc_name,
            dimensions = dimensions,
            # composed once at the container, so every child agrees
            name = name,
            # the child cannot resolve the parent's frame by name, so it is
            # handed the frame narrowed to itself
            space = space[nm],
            verbose = verbose, return_gobject = TRUE, output = output, ...)
    }
    gobject
}

# One job over a combinedSpace's members.
#
# Membership is derived from the recipe's steps (`names()`), never from
# `names(@objects)` -- a space that claimed every child would restate the
# object rather than declare anything. A sample that belongs to the layout
# without moving says so with a `member` step.
#' @noRd
.csn_combined <- function(gobject, param, method, parameters, space,
    spat_unit, spat_loc_name, dimensions, name, verbose,
    return_gobject, output, ...) {
    members <- names(space)
    if (length(members) == 0L) {
        stop("[createSpatialNetwork] combinedSpace '", space@name,
            "' names no samples, so there is nothing to build over. ",
            "Record a transform onto it, or declare membership with a ",
            "member step.", call. = FALSE)
    }
    unknown <- setdiff(members, names(gobject@objects))
    if (length(unknown) > 0L) {
        stop("[createSpatialNetwork] combinedSpace '", space@name,
            "' names sample(s) absent from the object: ",
            toString(unknown), call. = FALSE)
    }

    # `.gm_fused_spatlocs()` owns the order that matters: scope the frame per
    # child, apply, promote IDs to `sample::id`, THEN fold. Folding first
    # would trip the duplicate-ID check, since children share local IDs.
    sl <- .gm_fused_spatlocs(gobject, space, coordinator = NULL,
        spat_unit = spat_unit, name = spat_loc_name, samples = members)
    if (is.null(sl)) {
        stop("[createSpatialNetwork] no spatial locations found for ",
            "sample(s): ", toString(members), call. = FALSE)
    }

    want <- if (identical(output, "data.table") && !return_gobject) {
        "data.table"
    } else {
        "spatialNetworkObj"
    }
    built <- .spatial_network_from_locs(sl, param,
        method = method, parameters = parameters, name = name,
        dimensions = dimensions, spat_unit = spat_unit,
        verbose = verbose, output = want, ...)

    # One artifact, so returning it is unambiguous -- unlike the per-sample
    # path, where there are N and no single answer.
    if (!return_gobject) return(built)

    gobject <- setSpatialNetwork(gobject, x = built,
        spat_unit = spat_unit, name = name, verbose = verbose)
    update_giotto_params(gobject,
        description = "_spatial_network", toplevel = 1L)
}


# Build a spatial network from spatial locations and an already-constructed
# networkParam. The gobject-free half of the job: it takes a `spatLocsObj`
# and hands back an object, so one builder serves both a single giotto's own
# locations and a giottoMulti's fused cross-sample locations, where the IDs
# are `sample::id` globals and there is no single gobject to read from.
# Everything that knows about slots stays in the caller.
#
# `spat_unit` is accepted rather than read off `sl` because the caller has
# already resolved the default and the two must not disagree.
#' @noRd
.spatial_network_from_locs <- function(sl,
    param,
    method,
    parameters,
    name,
    dimensions = "all",
    spat_unit = NULL,
    verbose = FALSE,
    output = c("spatialNetworkObj", "data.table"),
    ...) {
    output <- match.arg(output, c("spatialNetworkObj", "data.table"))
    sl_dt <- sl[]
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    if (!identical(dimensions, "all")) coord_cols <- coord_cols[dimensions]
    coords <- as.matrix(sl_dt[, coord_cols, with = FALSE])

    g_net <- createNetwork(coords, param,
        node_ids = sl_dt$cell_ID, verbose = verbose, ...
    )

    if (identical(output, "data.table")) {
        return(data.table::as.data.table(
            igraph::as_data_frame(g_net, what = "edges")
        ))
    }

    create_spat_net_obj(
        name = name,
        method = method,
        parameters = parameters,
        network = g_net,
        spat_unit = spat_unit %null% spatUnit(sl),
        provenance = prov(sl)
    )
}


# Build a spatial network on cell centroids from an already-constructed
# networkParam, and do the gobject plumbing around it.
#
# THE one place all three doors converge: createSpatialNetwork(),
# createSpatialKNNnetwork() and createSpatialDelaunayNetwork() all route
# here with `param` already built. So this is where the frame reaches the
# name, and where a giottoMulti's job size is read off the space -- doing
# either in a caller means doing it three times and drifting twice.
#
# `name` is NULL until composed here, which is why the two wrappers declare
# `name = NULL` rather than their literal defaults: an eager default fires
# before this function can tell a user-supplied name from a fallback, and
# the frame prefix would then be dropped exactly when a frame was used.
.create_spatial_network_from_param <- function(gobject,
    param,
    method,
    parameters,
    spat_unit = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    name = NULL,
    default_name = NULL,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    space = NULL,
    ...) {
    output <- match.arg(output, c("spatialNetworkObj", "data.table"))
    spat_unit <- set_default_spat_unit(gobject, spat_unit = spat_unit)
    sp <- .resolve_space(gobject, space)

    # Default name: `<method>_network` (or the wrapper's own spelling),
    # prefixed by the frame when one was given. The frame has to reach the
    # name because it changes the artifact -- `rescale`, `shear` and a
    # general `affine` change distances, so `maximum_distance` and `radius`
    # mean different things in each, and shear changes Delaunay topology
    # outright -- and two frames must not collide on one name. Prefixing
    # rather than replacing keeps framed and native names the same kind of
    # thing, so the method is still readable off either. An explicit
    # `name =` is taken as given.
    #
    # (`spin` / `flip` / `spatShift` are isometries and leave the network
    # identical; the default does not try to detect that, because whether a
    # frame happens to be rigid is not something a name should depend on.)
    #
    # The native frame is exempt: naming it explicitly must produce the same
    # artifact as omitting `space`, or the two ways of saying "where the
    # data already is" would write to different names.
    #
    # `name` is the ONLY key a frame may compose into. Never `spat_unit`:
    # that keys expression, metadata and every nesting axis, so a
    # frame-named unit forks the object into the same cells twice. A frame
    # moves cells, it does not create them. See adr/0006.
    if (is.null(name)) {
        name <- default_name %null% paste0(method, "_network")
        if (!.is_native_space(sp)) name <- paste0(sp@name, "_", name)
    }

    # The native frame records as NA, however the caller spelled it: an
    # artifact built there is indistinguishable from one built with no
    # `space` at all, because it is the same artifact.
    #
    # Recorded in `parameters`, not in `@provenance`: that slot answers
    # "which spat_units were aggregated to make this", a different question,
    # and two of its consumers assume an atomic value.
    #
    # Assigned rather than appended, so that the per-sample path -- which
    # re-enters this function once per child -- records one `space` and not
    # a second one beside it. The child computes the same value from the
    # frame narrowed to itself, so the write is idempotent.
    parameters[["space"]] <-
        if (.is_native_space(sp)) NA_character_ else sp@name

    # giottoMulti: the space says how big the job is (adr/0006).
    if (inherits(gobject, "giottoMulti")) {
        return(.csn_multi(gobject, param = param, method = method,
            parameters = parameters, space = sp, spat_unit = spat_unit,
            spat_loc_name = spat_loc_name, dimensions = dimensions,
            name = name, verbose = verbose,
            return_gobject = return_gobject, output = output, ...))
    }

    sl <- getSpatialLocations(gobject,
        spat_unit = spat_unit, name = spat_loc_name,
        output = "spatLocsObj"
    )
    if (!.is_native_space(sp)) {
        sl <- .apply_space_to_subobj(sl, gobject, sp, coordinator = NULL)
    }

    # An edge table is only ever the answer when there is no gobject to write
    # into; with `return_gobject = TRUE` the object is built and set either way.
    want <- if (identical(output, "data.table") && !return_gobject) {
        "data.table"
    } else {
        "spatialNetworkObj"
    }
    built <- .spatial_network_from_locs(sl, param,
        method = method, parameters = parameters, name = name,
        dimensions = dimensions, spat_unit = spat_unit,
        verbose = verbose, output = want, ...
    )
    if (identical(want, "data.table")) return(built)
    sn_obj <- built

    if (!return_gobject) return(sn_obj)

    # setSpatialNetwork() reports an overwrite itself, so there is no name
    # check here. The older wrappers above still carry one and say it twice.
    gobject <- setSpatialNetwork(gobject,
        x = sn_obj, spat_unit = spat_unit, name = name, verbose = verbose
    )
    update_giotto_params(gobject,
        description = "_spatial_network", toplevel = 1L
    )
}





#' @title annotateSpatialNetwork
#' @name annotateSpatialNetwork
#' @description Annotate spatial network with cell metadata information.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spatial_network_name name of spatial network to use
#' @param cluster_column name of column to use for clusters
#' @param create_full_network convert from reduced to full network
#' representation
#' @returns annotated network in data.table format
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' annotateSpatialNetwork(g, cluster_column = "leiden_clus")
#' @export
annotateSpatialNetwork <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spatial_network_name = "Delaunay_network",
    cluster_column,
    create_full_network = FALSE) {
    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # get network
    if (!spatial_network_name %in%
        list_spatial_networks_names(gobject, spat_unit)) {
        stop(
            "\n spatial network with name: ",
            spatial_network_name, " does not exist \n"
        )
    }
    spatial_network <- getSpatialNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spatial_network_name,
        output = "networkDT"
    )

    if (isTRUE(create_full_network)) {
        # expand canonical (from, to) to both directions
        rev <- data.table::copy(spatial_network)
        data.table::setnames(rev, c("from", "to"), c("to", "from"))
        spatial_network <- unique(rbind(spatial_network, rev))
    }

    # Attach sdim*_begin / sdim*_end coords from spatLocsObj. Networks
    # no longer cache coords (as of 0.6.0), so we join them here for
    # downstream consumers that draw line segments. These are read from
    # the live spatLocsObj so any spatial transforms automatically
    # propagate.
    sl_dt <- getSpatialLocations(gobject,
        spat_unit = spat_unit, output = "data.table"
    )
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    sl_keys <- sl_dt[, c("cell_ID", coord_cols), with = FALSE]
    begin_cols <- paste0(coord_cols, "_begin")
    end_cols <- paste0(coord_cols, "_end")

    spatial_network <- merge(
        spatial_network,
        data.table::setnames(
            data.table::copy(sl_keys),
            c("cell_ID", coord_cols),
            c("from", begin_cols)
        ),
        by = "from"
    )
    spatial_network <- merge(
        spatial_network,
        data.table::setnames(
            data.table::copy(sl_keys),
            c("cell_ID", coord_cols),
            c("to", end_cols)
        ),
        by = "to"
    )



    # cell metadata
    cell_metadata <- getCellMetadata(gobject,
        feat_type = feat_type,
        spat_unit = spat_unit,
        output = "data.table",
        copy_obj = TRUE
    )
    if (!cluster_column %in% colnames(cell_metadata)) {
        stop("\n the cluster column does not exist in pDataDT(gobject) \n")
    }
    cluster_type_vector <- cell_metadata[[cluster_column]]
    names(cluster_type_vector) <- cell_metadata[["cell_ID"]]

    # data.table variables
    to_cell_type <- to <- from_cell_type <- from <- type_int <- from_to <- NULL

    spatial_network_annot <- data.table::copy(spatial_network)
    spatial_network_annot[, to_cell_type := cluster_type_vector[to]]
    spatial_network_annot[, from_cell_type := cluster_type_vector[from]]
    spatial_network_annot[
        ,
        type_int := ifelse(to_cell_type == from_cell_type, "homo", "hetero")
    ]

    # specific direction
    spatial_network_annot[
        ,
        from_to := paste0(from_cell_type, "-", to_cell_type)
    ]

    # unified direction, due to 'sort'
    spatial_network_annot <- dt_sort_combine_two_columns(spatial_network_annot,
        column1 = "from_cell_type",
        column2 = "to_cell_type",
        myname = "unified_int"
    )

    return(spatial_network_annot)
}





# spatial weight matrix ####
# TODO move to Giotto?

#' @title Create a spatial weight matrix
#' @name createSpatialWeightMatrix
#' @description Generate spatial weight matrix based on the strength of spatial
#' interactions between nodes. Requires spatial networks to be first generated.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param spatial_network_to_use spatial network information to use
#' @param method type of weighted matrix to generate. See details
#' @param wm_name name to assign the weight matrix values
#' @param return_gobject (default = TRUE) whether to return as the giotto object
#' with attached results or the bare weighted matrix
#' @param verbose be verbose
#' @returns spatial weight matrix
#' @details
#' \itemize{
#'   \item{\code{"distance"} method is calculated using 1/(1+distance) to
#'   create an inverse weighting based on the distance between nodes.}
#'   \item{\code{"adjacency"} method is a binary matrix with 1 signifying that
#'   two nodes are connected in the spatial network and 0 indicating that
#'   they are not.}
#' }
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialWeightMatrix(g, spatial_network_to_use = "spatial_network")
#' @export
createSpatialWeightMatrix <- function(gobject,
    spat_unit = NULL,
    spatial_network_to_use = "kNN_network",
    method = c("distance", "adjacency"),
    wm_name = "spat_weights",
    return_gobject = TRUE,
    verbose = TRUE) {
    # 1. setup
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    method <- match.arg(method, choices = c("distance", "adjacency"))

    sn <- getSpatialNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spatial_network_to_use,
        output = "spatialNetworkObj"
    )
    if (is.null(sn)) stop("Specified spatial network not found")

    # 2. calculate weights — sn[] is the canonical igraph
    g <- sn[]
    wm <- switch(method,
        "distance"  = igraph::as_adjacency_matrix(g, attr = "weight", sparse = TRUE),
        "adjacency" = igraph::as_adjacency_matrix(g, sparse = TRUE)
    )

    # 3. return results
    if (isTRUE(return_gobject)) {
        sn@misc$weight_matrix[[wm_name]] <- wm
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        if (isTRUE(verbose)) {
            wrap_msg("Attaching weight matrix to", spatial_network_to_use)
        }
        gobject <- setSpatialNetwork(
            gobject = gobject,
            x = sn,
            set_defaults = FALSE,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        return(gobject)
    } else {
        return(wm)
    }
}







## Spatial grid ####

#' @title .find_grid_3d
#' @name .find_grid_3d
#' @description find grid location in 3D
#' @keywords internal
#' @returns character
.find_grid_3d <- function(grid_DT, x_loc, y_loc, z_loc) {
    # data.table variables
    x_start <- x_end <- y_start <- y_end <- z_start <- z_end <- NULL

    name <- grid_DT[x_loc > x_start & x_loc < x_end & y_loc > y_start &
        y_loc < y_end & z_loc > z_start & z_loc < z_end]$gr_name
    return(name)
}

#' @title .find_grid_2d
#' @name .find_grid_2d
#' @description find grid location in 2D
#' @keywords internal
#' @returns character
.find_grid_2d <- function(grid_DT, x_loc, y_loc) {
    # data.table variables
    x_start <- x_end <- y_start <- y_end <- NULL

    name <- grid_DT[x_loc > x_start & x_loc < x_end & y_loc > y_start &
        y_loc < y_end]$gr_name
    return(name)
}

#' @title .find_grid_x
#' @name .find_grid_x
#' @description find grid location on x-axis
#' @keywords internal
#' @returns character
.find_grid_x <- function(grid_DT, x_loc) {
    # data.table variables
    x_start <- x_end <- gr_x_name <- NULL

    grid_DT_x <- unique(grid_DT[, .(x_start, x_end, gr_x_name)])
    name_x <- grid_DT_x[x_loc > x_start & x_loc < x_end]$gr_x_name
    return(name_x)
}

#' @title .find_grid_y
#' @name .find_grid_y
#' @description find grid location on y-axis
#' @keywords internal
#' @returns character
.find_grid_y <- function(grid_DT, y_loc) {
    # data.table variables
    y_start <- y_end <- gr_y_name <- NULL

    grid_DT_y <- unique(grid_DT[, .(y_start, y_end, gr_y_name)])
    name_y <- grid_DT_y[y_loc > y_start & y_loc < y_end]$gr_y_name
    return(name_y)
}

#' @title .find_grid_z
#' @name .find_grid_z
#' @description find grid location on z-axis
#' @keywords internal
#' @returns character
.find_grid_z <- function(grid_DT, z_loc) {
    # data.table variables
    z_start <- z_end <- gr_z_name <- NULL

    grid_DT_z <- unique(grid_DT[, .(z_start, z_end, gr_z_name)])
    name_z <- grid_DT_z[z_loc > z_start & z_loc < z_end]$gr_z_name
    return(name_z)
}



#' @title .create_spatialgrid_default_2d
#' @description create a 2D spatial grid
#' @keywords internal
#' @returns 2D spatial grid
.create_spatialgrid_default_2d <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    minimum_padding = 1) {
    # data.table variables
    gr_name <- gr_x_name <- gr_y_name <- gr_x_loc <- gr_y_loc <- gr_loc <- NULL

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    spatlocs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    )

    if (is.null(spatlocs)) {
        stop("\n spatial locations are needed to create a spatial grid \n")
    }

    ## calculate sequences for desired stepsize
    # x-axis
    x_range <- range(spatlocs$sdimx)
    x_start <- x_range[[1]] - minimum_padding
    x_end <- x_range[[2]] + minimum_padding
    dimx_steps <- ceiling((x_end - x_start) / sdimx_stepsize)
    dimx_start <- mean(c(x_start, x_end)) - ((dimx_steps / 2) * sdimx_stepsize)
    dimx_end <- mean(c(x_start, x_end)) + ((dimx_steps / 2) * sdimx_stepsize)
    my_x_seq <- seq(from = dimx_start, to = dimx_end, by = sdimx_stepsize)

    # y-axis
    y_range <- range(spatlocs$sdimy)
    y_start <- y_range[[1]] - minimum_padding
    y_end <- y_range[[2]] + minimum_padding
    dimy_steps <- ceiling((y_end - y_start) / sdimy_stepsize)
    dimy_start <- mean(c(y_start, y_end)) - ((dimy_steps / 2) * sdimy_stepsize)
    dimy_end <- mean(c(y_start, y_end)) + ((dimy_steps / 2) * sdimy_stepsize)
    my_y_seq <- seq(from = dimy_start, to = dimy_end, by = sdimy_stepsize)


    ## create grid with starts and ends
    grid_starts <- data.table::as.data.table(expand.grid(
        my_x_seq[-length(my_x_seq)],
        my_y_seq[-length(my_y_seq)]
    ))
    colnames(grid_starts) <- c("x_start", "y_start")
    grid_ends <- data.table::as.data.table(expand.grid(
        my_x_seq[-1],
        my_y_seq[-1]
    ))
    colnames(grid_ends) <- c("x_end", "y_end")
    spatgrid <- cbind(grid_starts, grid_ends)


    ## first label the grid itself ##
    spatgrid[, gr_name := paste0("gr_", seq_len(.N))]

    # x-axis
    x_labels <- sort(unique(spatgrid$x_start))
    x_gr_names <- paste0("gr_x_", seq_len(length(x_labels)))
    names(x_gr_names) <- x_labels
    x_gr_names_vector <- x_gr_names[as.character(spatgrid$x_start)]
    spatgrid[, gr_x_name := x_gr_names_vector]

    # y-axis
    y_labels <- sort(unique(spatgrid$y_start))
    y_gr_names <- paste0("gr_y_", seq_len(length(y_labels)))
    names(y_gr_names) <- y_labels
    y_gr_names_vector <- y_gr_names[as.character(spatgrid$y_start)]
    spatgrid[, gr_y_name := y_gr_names_vector]

    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(spatgrid$gr_x_name, "-", spatgrid$gr_y_name)


    return(spatgrid)
}


#' @title .create_spatialgrid_default_3d
#' @description create a 3D spatial grid
#' @keywords internal
#' @returns 3D spatial grid
.create_spatialgrid_default_3d <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1) {
    # data.table variables
    gr_name <- gr_x_name <- gr_y_name <- gr_z_name <- gr_x_loc <-
        gr_y_loc <- gr_z_loc <- gr_loc <- NULL

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    spatlocs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    )

    if (is.null(spatlocs)) {
        stop("\n spatial locations are needed to create a spatial grid \n")
    }

    ## calculate sequences for desired stepsize
    # x-axis
    x_range <- range(spatlocs$sdimx)
    x_start <- x_range[[1]] - minimum_padding
    x_end <- x_range[[2]] + minimum_padding
    dimx_steps <- ceiling((x_end - x_start) / sdimx_stepsize)
    dimx_start <- mean(c(x_start, x_end)) - ((dimx_steps / 2) * sdimx_stepsize)
    dimx_end <- mean(c(x_start, x_end)) + ((dimx_steps / 2) * sdimx_stepsize)
    my_x_seq <- seq(from = dimx_start, to = dimx_end, by = sdimx_stepsize)

    # y-axis
    y_range <- range(spatlocs$sdimy)
    y_start <- y_range[[1]] - minimum_padding
    y_end <- y_range[[2]] + minimum_padding
    dimy_steps <- ceiling((y_end - y_start) / sdimy_stepsize)
    dimy_start <- mean(c(y_start, y_end)) - ((dimy_steps / 2) * sdimy_stepsize)
    dimy_end <- mean(c(y_start, y_end)) + ((dimy_steps / 2) * sdimy_stepsize)
    my_y_seq <- seq(from = dimy_start, to = dimy_end, by = sdimy_stepsize)

    # z-axis
    z_range <- range(spatlocs$sdimz)
    z_start <- z_range[[1]] - minimum_padding
    z_end <- z_range[[2]] + minimum_padding
    dimz_steps <- ceiling((z_end - z_start) / sdimz_stepsize)
    dimz_start <- mean(c(z_start, z_end)) - ((dimz_steps / 2) * sdimz_stepsize)
    dimz_end <- mean(c(z_start, z_end)) + ((dimz_steps / 2) * sdimz_stepsize)
    my_z_seq <- seq(from = dimz_start, to = dimz_end, by = sdimz_stepsize)

    ## create grid with starts and ends
    grid_starts <- data.table::as.data.table(expand.grid(
        my_x_seq[-length(my_x_seq)],
        my_y_seq[-length(my_y_seq)],
        my_z_seq[-length(my_z_seq)]
    ))
    colnames(grid_starts) <- c("x_start", "y_start", "z_start")
    grid_ends <- data.table::as.data.table(expand.grid(
        my_x_seq[-1],
        my_y_seq[-1],
        my_z_seq[-1]
    ))
    colnames(grid_ends) <- c("x_end", "y_end", "z_end")
    spatgrid <- cbind(grid_starts, grid_ends)


    ## first label the grid itself ##
    spatgrid[, gr_name := paste0("gr_", seq_len(.N))]

    # x-axis
    x_labels <- sort(unique(spatgrid$x_start))
    x_gr_names <- paste0("gr_x_", seq_len(length(x_labels)))
    names(x_gr_names) <- x_labels
    x_gr_names_vector <- x_gr_names[as.character(spatgrid$x_start)]
    spatgrid[, gr_x_name := x_gr_names_vector]

    # y-axis
    y_labels <- sort(unique(spatgrid$y_start))
    y_gr_names <- paste0("gr_y_", seq_len(length(y_labels)))
    names(y_gr_names) <- y_labels
    y_gr_names_vector <- y_gr_names[as.character(spatgrid$y_start)]
    spatgrid[, gr_y_name := y_gr_names_vector]

    # z-axis
    z_labels <- sort(unique(spatgrid$z_start))
    z_gr_names <- paste0("gr_z_", seq_len(length(z_labels)))
    names(z_gr_names) <- z_labels
    z_gr_names_vector <- z_gr_names[as.character(spatgrid$z_start)]
    spatgrid[, gr_z_name := z_gr_names_vector]

    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(
        spatgrid$gr_x_name, "-",
        spatgrid$gr_y_name, "-", spatgrid$gr_z_name
    )

    return(spatgrid)
}



#' @title createSpatialDefaultGrid
#' @name createSpatialDefaultGrid
#' @description Create a spatial grid using the default method
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name spatial location name
#' @param sdimx_stepsize stepsize along the x-axis
#' @param sdimy_stepsize stepsize along the y-axis
#' @param sdimz_stepsize stepsize along the z-axis
#' @param minimum_padding minimum padding on the edges
#' @param name name for spatial grid (default = 'spatial_grid')
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object with updated spatial grid slot
#' @details Creates a spatial grid with defined x, y (and z) dimensions.
#' The dimension units are based on the provided spatial location units.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialDefaultGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#' @export
createSpatialDefaultGrid <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1,
    name = NULL,
    return_gobject = TRUE) {
    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # check parameters
    if (is.null(name)) {
        name <- "spatial_grid"
    }

    if (length(c(sdimx_stepsize, sdimy_stepsize, sdimz_stepsize)) == 3) {
        resultgrid <- .create_spatialgrid_default_3d(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            sdimz_stepsize = sdimz_stepsize,
            minimum_padding = minimum_padding
        )
    } else if (!is.null(sdimx_stepsize) & !is.null(sdimy_stepsize)) {
        resultgrid <- .create_spatialgrid_default_2d(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            minimum_padding = minimum_padding
        )
    } else {
        stop("\n the stepsize for the x-axis (sdimx) and y-axis (sdimy) is
            the minimal requirement \n\n Additionally for a 3D spatial grid
            the z-axis (sdimz) is also required \n")
    }


    # object return
    if (return_gobject == TRUE) {
        # 1. check if name has already been used
        spg_names <- list_spatial_grids_names(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type
        )

        if (name %in% spg_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        # 2. create spatial grid object
        parameters <- list(
            "sdimx_stepsize" = sdimx_stepsize,
            "sdimy_stepsize" = sdimy_stepsize,
            "sdimz_stepsize" = sdimz_stepsize,
            "minimum_padding" = minimum_padding
        )

        spatgridobj <- new("spatialGridObj",
            name = name,
            method = "default",
            parameters = parameters,
            gridDT = resultgrid,
            # outputObj = NULL, # NULL with default
            # (from original S3 definition)
            spat_unit = spat_unit,
            feat_type = feat_type,
            misc = NULL
        )

        # 3. assign spatial grid object
        gobject <- setSpatialGrid(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            name = name,
            spatial_grid = spatgridobj
        )

        # 4. update log
        ## update parameters used ##

        # parent function name
        cl <- sys.call(-1)


        if (is.null(cl)) {
            gobject <- update_giotto_params(gobject, description = "_grid")
        } else {
            fname <- as.character(cl[[1]])
            if (fname == "createSpatialGrid") {
                gobject <- update_giotto_params(gobject,
                    description = "_grid",
                    toplevel = 3
                )
            } else {
                gobject <- update_giotto_params(gobject, description = "_grid")
            }
        }

        return(gobject)
    } else {
        return(resultgrid)
    }
}





#' @title createSpatialGrid
#' @name createSpatialGrid
#' @description Create a spatial grid using the default method
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param spat_loc_name spatial location name
#' @param name name for spatial grid
#' @param method method to create a spatial grid
#' @param sdimx_stepsize stepsize along the x-axis
#' @param sdimy_stepsize stepsize along the y-axis
#' @param sdimz_stepsize stepsize along the z-axis
#' @param minimum_padding minimum padding on the edges
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object with updated spatial grid slot
#' @details Creates a spatial grid with defined x, y (and z) dimensions.
#' The dimension units are based on the provided spatial location units.
#'   * **default method:** \code{\link{createSpatialDefaultGrid}}
#'
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' @export
createSpatialGrid <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    name = NULL,
    method = c("default"),
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1,
    return_gobject = TRUE) {
    # get parameters
    method <- match.arg(method, c("default"))

    if (method == "default") {
        out <- createSpatialDefaultGrid(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            sdimz_stepsize = sdimz_stepsize,
            minimum_padding = minimum_padding,
            name = name,
            return_gobject = return_gobject
        )
    }

    return(out)
}







#' @title annotate_spatlocs_with_spatgrid_2D
#' @description annotate spatial locations with 2D spatial grid information
#' @param spatloc spatial_locs slot from giotto object
#' @param spatgrid selected spatial_grid slot from giotto object
#' @returns annotated spatial location data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#' g_spatloc <- getSpatialLocations(g, output = "data.table")
#' g_spatgrid <- getSpatialGrid(g)
#'
#' annotate_spatlocs_with_spatgrid_2D(
#'     spatloc = g_spatloc,
#'     spatgrid = g_spatgrid
#' )
#' @export
annotate_spatlocs_with_spatgrid_2D <- function(spatloc,
    spatgrid) {
    ## second label the spatial locations ##
    spatlocs <- data.table::copy(spatloc)

    # data.table variables
    gr_x_loc <- gr_y_loc <- gr_loc <- NULL

    x_vector <- spatlocs$sdimx
    x_breaks <- sort(unique(spatgrid$x_end))
    x_breaks_labels <- paste0("gr_x_", seq_len(length(x_breaks)))
    minimum_x <- min(spatgrid$x_start)
    my_x_gr <- cut(
        x = x_vector, breaks = c(minimum_x, x_breaks),
        include.lowest = TRUE, right = TRUE, labels = x_breaks_labels
    )
    spatlocs[, gr_x_loc := as.character(my_x_gr)]

    y_vector <- spatlocs$sdimy
    y_breaks <- sort(unique(spatgrid$y_end))
    y_breaks_labels <- paste0("gr_y_", seq_len(length(y_breaks)))
    minimum_y <- min(spatgrid$y_start)
    my_y_gr <- cut(
        x = y_vector, breaks = c(minimum_y, y_breaks),
        include.lowest = TRUE, right = TRUE, labels = y_breaks_labels
    )
    spatlocs[, gr_y_loc := as.character(my_y_gr)]


    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(spatgrid$gr_x_name, "-", spatgrid$gr_y_name)

    indiv_dim_names <- paste0(spatlocs$gr_x_loc, "-", spatlocs$gr_y_loc)
    my_gr <- gr_dim_names[indiv_dim_names]
    spatlocs[, gr_loc := as.character(my_gr)]

    return(spatlocs)
}


#' @title annotate_spatlocs_with_spatgrid_3D
#' @description annotate spatial locations with 3D spatial grid information
#' @param spatloc spatial_locs slot from giotto object
#' @param spatgrid selected spatial_grid slot from giotto object
#' @returns annotated spatial location data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("starmap")
#' g_spatloc <- getSpatialLocations(g, output = "data.table")
#' g_spatgrid <- getSpatialGrid(g)
#'
#' annotate_spatlocs_with_spatgrid_3D(
#'     spatloc = g_spatloc,
#'     spatgrid = g_spatgrid
#' )
#' @export
annotate_spatlocs_with_spatgrid_3D <- function(spatloc,
    spatgrid) {
    ## second label the spatial locations ##
    spatlocs <- data.table::copy(spatloc)

    # data.table variables
    gr_x_loc <- gr_y_loc <- gr_z_loc <- gr_loc <- NULL

    x_vector <- spatlocs$sdimx
    x_breaks <- sort(unique(spatgrid$x_end))
    x_breaks_labels <- paste0("gr_x_", seq_len(length(x_breaks)))
    minimum_x <- min(spatgrid$x_start)
    my_x_gr <- cut(
        x = x_vector, breaks = c(minimum_x, x_breaks),
        include.lowest = TRUE, right = TRUE, labels = x_breaks_labels
    )
    spatlocs[, gr_x_loc := as.character(my_x_gr)]

    y_vector <- spatlocs$sdimy
    y_breaks <- sort(unique(spatgrid$y_end))
    y_breaks_labels <- paste0("gr_y_", seq_len(length(y_breaks)))
    minimum_y <- min(spatgrid$y_start)
    my_y_gr <- cut(
        x = y_vector, breaks = c(minimum_y, y_breaks),
        include.lowest = TRUE, right = TRUE, labels = y_breaks_labels
    )
    spatlocs[, gr_y_loc := as.character(my_y_gr)]

    z_vector <- spatlocs$sdimz
    z_breaks <- sort(unique(spatgrid$z_end))
    z_breaks_labels <- paste0("gr_z_", seq_len(length(z_breaks)))
    minimum_z <- min(spatgrid$z_start)
    my_z_gr <- cut(
        x = z_vector, breaks = c(minimum_z, z_breaks),
        include.lowest = TRUE, right = TRUE, labels = z_breaks_labels
    )
    spatlocs[, gr_z_loc := as.character(my_z_gr)]


    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(
        spatgrid$gr_x_name, "-", spatgrid$gr_y_name, "-", spatgrid$gr_z_name
    )

    indiv_dim_names <- paste0(
        spatlocs$gr_x_loc, "-", spatlocs$gr_y_loc, "-", spatlocs$gr_z_loc
    )
    my_gr <- gr_dim_names[indiv_dim_names]
    spatlocs[, gr_loc := as.character(my_gr)]

    return(spatlocs)
}




#' @title annotateSpatialGrid
#' @name annotateSpatialGrid
#' @description annotate spatial grid with cell ID and cell metadata (optional)
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations
#' @param spatial_grid_name name of spatial grid,
#' see \code{\link{showGiottoSpatGrids}}
#' @param cluster_columns names of cell metadata, see \code{\link{pDataDT}}
#' @returns annotated spatial grid data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' annotateSpatialGrid(g)
#' @export
annotateSpatialGrid <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = "raw",
    spatial_grid_name = "spatial_grid",
    cluster_columns = NULL) {
    # get grid
    spatial_grid <- getSpatialGrid(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        name = spatial_grid_name
    )
    spatial_locs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    ) # copy happens anyways in step 1

    # 1. annotate spatial grid with spatial locations
    if (all(c("sdimx", "sdimy", "sdimz") %in% colnames(spatial_locs))) {
        annotgrid_locs <- annotate_spatlocs_with_spatgrid_3D(
            spatloc = spatial_locs, spatgrid = spatial_grid
        )
    } else if (all(c("sdimx", "sdimy") %in% colnames(spatial_locs))) {
        annotgrid_locs <- annotate_spatlocs_with_spatgrid_2D(
            spatloc = spatial_locs, spatgrid = spatial_grid
        )
    }

    # 2.select metadata
    cell_metadata <- pDataDT(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    if (!is.null(cluster_columns)) {
        annotation_vector <- cluster_columns
        possible_annotations <- colnames(cell_metadata)

        missing_annotation <- annotation_vector[!annotation_vector %in%
            possible_annotations]
        if (length(missing_annotation) > 0) {
            wrap_msg("These annotations were not found back in the cell metadata
                (pDataDT): \n", missing_annotation)
        }

        annotation_vector_found <- annotation_vector[annotation_vector %in%
            possible_annotations]
        cell_meta_selected <- cell_metadata[,
            c("cell_ID", annotation_vector_found),
            with = FALSE
        ]

        annotated_grid <- data.table::merge.data.table(
            x = annotgrid_locs, y = cell_meta_selected, by = "cell_ID"
        )

        return(annotated_grid)
    } else {
        return(annotgrid_locs)
    }
}
