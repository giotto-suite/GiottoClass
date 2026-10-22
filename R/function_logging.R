#### logging of giotto functions ####


# Functions to find the previous function name or args have been moved to
# GiottoUtils prev_call.R


#' @title Update giotto parameters
#' @name update_giotto_params
#' @description
#' For developer use. Adds an entry to the `giotto` object object history.
#' Care currently needs to be taken when a function that contains a call to
#' this function is called from within yet another function. In such cases,
#' a `toplevel < 0` or setting a temporary `"giotto.update_param" = FALSE` with
#' `GiottoUtils::gwith_option()` may be the best option to avoid either
#' evaluation errors or strange history entries. A new `update_giotto_params()`
#' call can then be added that describes the function of the topmost function
#' if desired.
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being
#' recorded was made. If negative, param recording is skipped
#' @param attachments named list. Items to attach. These are intended for lightweight
#' param classes containing settings. No large items should be added here.
#' @returns giotto object or list of parameters
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' update_giotto_params(g, toplevel = 1)
#' @export
update_giotto_params <- function(
        gobject,
        description = "_test",
        return_gobject = TRUE,
        toplevel = 2,
        attachments = NULL) {
    checkmate::assert_list(attachments, null.ok = TRUE, names = "unique")
    parameters_list <- gobject@parameters
    number_of_rounds <- length(parameters_list)
    update_name <- paste0(number_of_rounds, description)

    # return before updating if toplevel negative or global setting is FALSE
    if (skip_update <- toplevel < 0 ||
        !getOption("giotto.update_param", TRUE)) {
        if (isTRUE(return_gobject)) {
            return(gobject)
        } else {
            return(list(
                plist = parameters_list,
                newname = names(tail(parameters_list, 1L))
            ))
        }
    }

    # `get_args()` can be problematic. Allow skip right before this step.

    # update parameters list
    new_entry <- get_args(toplevel = toplevel)
    class(new_entry) <- c("ghistory_item", "character")
    if (!is.null(attachments)) {
        attr(new_entry, "attachments") <- attachments
    }

    # Structured record, carried as an attribute so the entry stays the
    # character vector every existing reader expects. `params` here are the
    # deparsed argument expressions, which keep `1:30` intact where the
    # flattened character form of `get_args()` reduces it to its first element.
    call_info <- .ghistory_call_info(toplevel = toplevel)
    attr(new_entry, "step") <- .ghistory_step(
        step_id = update_name,
        fn = call_info$fn,
        params = call_info$params
    )

    parameters_list[[update_name]] <- new_entry
    class(parameters_list) <- "ghistory"

    if (isTRUE(return_gobject)) {
        gobject@parameters <- parameters_list
        return(gobject)
    } else {
        return(list(plist = parameters_list, newname = update_name))
    }
}

#' @export
#' @keywords internal
print.ghistory <- function(x, ...) {
    message("Steps and parameters used:")
    for (i in seq_along(x)) {
        cat(GiottoUtils::color_blue(sprintf("<%s>\n", names(x)[[i]])))
        item <- x[[i]]
        # objHistory() records; @parameters itself holds ghistory_items
        if (is.list(item) && !is.null(item$step_id)) {
            .print_ghistory_record(item)
        } else {
            print(item)
        }
    }
}

# a subset of a history is still a history, so head() / tail() print as one
#' @export
#' @keywords internal
`[.ghistory` <- function(x, i) {
    structure(unclass(x)[i], class = class(x))
}

.print_ghistory_record <- function(r) {
    if (length(r$params)) {
        GiottoUtils::print_list(unlist(r$params), pre = "  ")
    }
    if (!identical(r$status, "ok")) {
        cat(GiottoUtils::color_yellow(sprintf("  status: %s\n", r$status)))
        if (!is.null(r$error)) cat(sprintf("  error: %s\n", r$error))
    }
}

#' @export
#' @keywords internal
print.ghistory_item <- function(x, ...) {
    GiottoUtils::print_list(x, pre = "  ")
    atts <- attr(x, "attachments")
    if (!is.null(atts)) {
        for (a in names(atts)) {
            cat(" ", GiottoUtils::color_yellow(sprintf("<%s> :\n", a)))
            cat(str_reformat(atts[[a]], indent = 4))
        }
    }
}

#' @title Giotto object history
#' @name objHistory
#' @description
#' The steps recorded on a giotto object: one record per step, holding the
#' function (`fn`), the parameters it ran with as deparsed expressions
#' (`params`), and `timestamp`, `seed`, `status`, `error` and `diff`. Steps
#' recorded before these records existed are reported with `status = "ok"`
#' and the parameters they recorded.
#'
#' The history can also be read back without loading the object: pass the
#' folder written by [saveGiotto()], or the path to its `history.ndjson`. The
#' result is the same as `objHistory()` on the object at save time.
#'
#' The history is provenance, not state: only functions that opt in are
#' recorded. What the object holds comes from [objManifest()].
#' @param x giotto object, or the path to a [saveGiotto()] folder or its
#' `history.ndjson`. A disk-backed project directory is read through its
#' source: pass `name` to pick a snapshot (the most recent by default).
#' @param summarized logical. Whether to also print a summary of the steps and
#' the names they used
#' @param output character. `"ghistory"` (default) returns the records as a
#' list; `"ndjson"` returns them as newline-delimited JSON, one step per line.
#' @param file character. Optional path to write the history to as NDJSON.
#' When given, the path is returned invisibly and `output` is ignored.
#' @param ... additional params (none implemented)
#' @returns list of class `ghistory`, an NDJSON string, or the `file` path
#' invisibly
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' objHistory(g)
#' objHistory(g, summarized = TRUE)
#' cat(substr(objHistory(g, output = "ndjson"), 1, 120))
#' @export
setMethod("objHistory", signature("gAny"), function(
        x, summarized = FALSE, output = c("ghistory", "ndjson"), file = NULL,
        ...) {
    output <- match.arg(output)
    .history_output(.history_build(x), summarized = summarized,
        output = output, file = file)
})

#' @rdname objHistory
#' @export
setMethod("objHistory", signature("character"), function(
        x, summarized = FALSE, output = c("ghistory", "ndjson"), file = NULL,
        ...) {
    output <- match.arg(output)
    # a project directory is read through its source, which finds the snapshot
    src <- .resolve_source(x)
    if (!isFALSE(src)) {
        return(objHistory(src, summarized = summarized, output = output,
            file = file, ...))
    }
    .history_output(.history_read(x), summarized = summarized,
        output = output, file = file)
})

#' @title showProcessingSteps
#' @name showProcessingSteps
#' @description shows the sequential processing steps that were performed
#' on a Giotto object in a summarized format
#' @param gobject giotto object
#' @returns list of processing steps and names
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showProcessingSteps(g)
#' @export
showProcessingSteps <- function(gobject) {
    deprecate_warn(
        when = "0.4.0",
        what = "showProcessingSteps()",
        with = "objHistory()",
        details = "objHistory with arg `summarized = TRUE` replaces this functionality"
    )

    parameters <- gobject@parameters

    message("Processing steps:")

    for (step in names(parameters)) {
        message(step)

        sub_step <- parameters[[step]]

        if (any(grepl("name", names(sub_step)) == TRUE)) {
            selected_names <- grep("name", names(sub_step), value = TRUE)
            wrap_msg("\t name info: ", sub_step[selected_names])
        }
    }
}


#### structured history records ####

# The op log answers "why does the object look like this". It is provenance,
# not state: coverage is partial (functions must opt in), args are recorded as
# written rather than as resolved, and a failed call historically left no trace
# at all. Object state therefore comes from `objManifest()` and is never
# reconstructed by replaying this log. See [manifestDiff()].

.GHISTORY_STATUS <- c("ok", "error", "unattributed")

# Name and deparsed argument expressions of the recorded call.
#
# `sys.call(-n)` counts evaluation frames, so this must walk the stack BEFORE
# entering any tryCatch: those add four frames of their own and the walk lands
# on `tryCatchOne` instead of the function being recorded. Called at the same
# depth as `get_args()` so both see the same frame.
.ghistory_call_info <- function(toplevel = 2L) {
    none <- list(fn = NA_character_, params = list())
    if (sys.nframe() <= toplevel) return(none)

    cl <- sys.call(-toplevel)
    f <- sys.function(-toplevel)
    if (is.null(cl)) return(none)

    fn <- tryCatch(
        {
            nm <- as.character(cl[[1]])
            # `pkg::fn(...)` deparses to c("::", "pkg", "fn")
            if (length(nm) > 1L) nm[[length(nm)]] else nm
        },
        error = function(e) NA_character_
    )
    params <- tryCatch(
        {
            mc <- match.call(definition = f, call = cl)
            # defaults first, then whatever the call supplied: the record is
            # the parameters the function actually ran with, not only the ones
            # typed out
            args <- formals(f)
            supplied <- as.list(mc)[-1]
            args[names(supplied)] <- supplied
            args <- args[names(args) != "..."]
            args <- args[!vapply(args, function(a) {
                is.symbol(a) && !nzchar(as.character(a))
            }, logical(1L))]
            lapply(args, function(a) paste(deparse(a), collapse = " "))
        },
        error = function(e) list()
    )
    list(fn = fn, params = params)
}

# Cheap RNG state marker: a full .Random.seed is ~626 integers, which has no
# place in a record meant to stay small. A digest of it is enough to tell two
# runs apart.
.ghistory_seed <- function() {
    tryCatch(
        {
            has_seed <- exists(
                ".Random.seed", envir = globalenv(), inherits = FALSE
            )
            if (!has_seed) return(NULL)
            if (!requireNamespace("digest", quietly = TRUE)) return(NULL)
            list(
                kind = RNGkind()[[1]],
                state = digest::digest(
                    get(".Random.seed", envir = globalenv()),
                    algo = "xxhash64"
                )
            )
        },
        error = function(e) NULL
    )
}

.ghistory_step <- function(step_id,
    fn = NA_character_,
    params = list(),
    status = "ok",
    diff = NULL,
    error = NULL) {
    status <- match.arg(status, .GHISTORY_STATUS)
    list(
        step_id = step_id,
        fn = fn,
        params = params,
        timestamp = format(
            as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"
        ),
        seed = .ghistory_seed(),
        status = status,
        error = error,
        diff = diff
    )
}

#' @title Record a giotto object history step
#' @name recordGiottoStep
#' @description
#' Append a history entry that `update_giotto_params()` cannot produce itself:
#' a call that failed, or a change made outside a logging function.
#'
#' Only 52 of the suite's functions call [update_giotto_params()], so a
#' manifest can move without any entry claiming it. An execution tool that
#' diffs [objManifest()] before and after a chunk closes that gap by recording
#' the unclaimed change here with `status = "unattributed"`, rather than
#' letting it vanish.
#' @param gobject giotto object
#' @param fn character. Name of the function or code that ran
#' @param params list. Parameters, as deparsed strings
#' @param status character. One of "ok", "error", "unattributed"
#' @param diff list. Manifest delta from [manifestDiff()]
#' @param error character. Error message, when `status = "error"`
#' @param description character. Suffix for the step name
#' @returns giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' g <- recordGiottoStep(g, fn = "manual edit", status = "unattributed")
#' tail(names(objHistory(g)), 1)
#' @export
recordGiottoStep <- function(gobject,
    fn = NA_character_,
    params = list(),
    status = c("ok", "error", "unattributed"),
    diff = NULL,
    error = NULL,
    description = NULL) {
    status <- match.arg(status)
    description <- description %||% paste0("_", status)

    parameters_list <- gobject@parameters
    update_name <- paste0(length(parameters_list), description)

    entry <- vapply(params, function(p) paste(as.character(p), collapse = " "),
        character(1L)
    )
    if (length(entry) == 0L) entry <- character(0)
    class(entry) <- c("ghistory_item", "character")
    attr(entry, "step") <- .ghistory_step(
        step_id = update_name, fn = fn, params = params,
        status = status, diff = diff, error = error
    )

    parameters_list[[update_name]] <- entry
    class(parameters_list) <- "ghistory"
    gobject@parameters <- parameters_list
    gobject
}

# Builds the records for one container class. objHistory() on gAny handles
# output, so a container with no method here fails at dispatch.
setGeneric(".history_build", function(x, ...) standardGeneric(".history_build"))

setMethod(".history_build", signature("giotto"), function(x, ...) {
    p <- x@parameters
    out <- lapply(names(p), function(nm) {
        item <- p[[nm]]
        step <- attr(item, "step")
        if (!is.null(step)) return(step)
        # legacy entry: no structured record was written at the time
        list(
            step_id = nm,
            fn = sub("^[0-9]+_", "", nm),
            params = as.list(unclass(item)),
            timestamp = NULL,
            seed = NULL,
            status = "ok",
            error = NULL,
            diff = NULL
        )
    })
    .as_ghistory(out)
})

.as_ghistory <- function(records) {
    names(records) <- vapply(records, function(r) r$step_id, character(1L))
    class(records) <- c("ghistory", "list")
    records
}

.history_output <- function(h, summarized = FALSE, output = "ghistory",
        file = NULL) {
    if (isTRUE(summarized)) .history_summary(h)
    if (!is.null(file)) {
        writeLines(.history_ndjson_lines(h), con = file)
        return(invisible(file))
    }
    if (identical(output, "ndjson")) {
        lines <- .history_ndjson_lines(h)
        if (!length(lines)) return("")
        return(paste0(paste(lines, collapse = "\n"), "\n"))
    }
    h
}

# step names, and any parameter naming what the step made
.history_summary <- function(h) {
    message("Processing steps:")
    for (r in h) {
        message(r$step_id)
        nm <- grep("name", names(r$params), value = TRUE)
        if (length(nm)) wrap_msg("\t name info: ", unlist(r$params[nm]))
    }
}

# One step per line, so writing later steps never rewrites earlier lines.
# Same encoding as the manifest JSON.
.history_ndjson_lines <- function(h) {
    GiottoUtils::package_check("jsonlite", repository = "CRAN:jsonlite")
    vapply(unname(unclass(h)), function(r) {
        as.character(jsonlite::toJSON(
            .manifest_json_prep(r),
            auto_unbox = TRUE, null = "null", na = "null",
            digits = I(17L), always_decimal = TRUE
        ))
    }, character(1L))
}

# A saveGiotto() folder, or a history.ndjson directly
.history_read <- function(path) {
    GiottoUtils::package_check("jsonlite", repository = "CRAN:jsonlite")
    checkmate::assert_string(path)
    f <- if (dir.exists(path)) file.path(path, "history.ndjson") else path
    if (!file.exists(f)) {
        stop("[objHistory] no history.ndjson in ", path, ". Objects saved ",
            "before the history was written have to be saved again.",
            call. = FALSE)
    }
    lines <- readLines(f, warn = FALSE)
    lines <- lines[nzchar(lines)]
    .as_ghistory(lapply(lines, function(l) {
        raw <- jsonlite::fromJSON(l, simplifyVector = FALSE)
        r <- .manifest_from_json(raw)
        # params are deparsed expressions, always text: the expression `NA`
        # is the string "NA", not a missing value
        if (length(raw$params)) r$params <- lapply(raw$params, as.character)
        r
    }))
}
