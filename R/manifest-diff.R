#' @include manifest.R
NULL

#### manifest diffs ####

# PURE: manifest in, manifest out. No giotto object is ever touched here, so
# this logic is unit-testable with synthetic manifests and cannot be broken by
# an accessor change.

# human-readable label per slot, used when phrasing a diff
.MANIFEST_SLOT_LABELS <- c(
    expression = "expression",
    spatial_locs = "spatial locations",
    spatial_info = "polygons",
    feat_info = "points",
    cell_metadata = "cell metadata",
    feat_metadata = "feature metadata",
    dimension_reduction = "dim reduction",
    nn_network = "NN network",
    spatial_network = "spatial network",
    spatial_enrichment = "spatial enrichment",
    images = "image",
    view = "view",
    spaces = "space",
    cell_ID = "cell IDs",
    feat_ID = "feature IDs"
)

# Key a manifest's `slots` records by identity, e.g.
# "expression.cell.rna.normalized", so two manifests can be paired record by
# record.
.manifest_flatten <- function(slots) {
    out <- list()
    for (sn in names(slots)) {
        for (rec in slots[[sn]]) {
            if (!is.list(rec)) next
            # Carry the display name alongside the identity. Splitting the id
            # back apart loses any key that itself contains a dot, and Giotto
            # names routinely do - an `sNN.pca` network read as "pca".
            keys <- rec$keys
            attr(rec, "key") <- keys[["name"]] %||%
                if (length(keys)) keys[[length(keys)]] else ""
            # a duplicate identity (flagged in the manifest's warnings) must
            # not overwrite its twin, or one record silently drops out of the
            # comparison
            id <- .manifest_item_id(sn, rec)
            n <- 1L
            base <- id
            while (!is.null(out[[id]])) {
                n <- n + 1L
                id <- paste0(base, "#", n)
            }
            out[[id]] <- rec
        }
    }
    out
}

.manifest_slot_of <- function(path) sub("\\..*$", "", path)

.manifest_label <- function(path) {
    slot <- .manifest_slot_of(path)
    .MANIFEST_SLOT_LABELS[[slot]] %||% slot
}

# trailing key: the name a user recognises ("normalized", "pca.RNA")
.manifest_leafname <- function(path) sub("^.*\\.", "", path)

# The key a leaf sits under - which is where a reader would go looking for it.
# Recorded during flattening; the path-tail fallback covers a leaf that reached
# here without one.
.manifest_display_name <- function(leaf, path) {
    key <- attr(leaf, "key")
    if (is.null(key) || !nzchar(key)) .manifest_leafname(path) else key
}

# columns of a metadata leaf, as a named vector of level counts
.manifest_colmap <- function(leaf) {
    cols <- leaf$columns
    if (is.null(cols) || length(cols) == 0L) {
        return(stats::setNames(integer(0), character(0)))
    }
    # column names only, for the slots whose columns are not described
    if (is.character(cols)) {
        return(stats::setNames(rep(NA_integer_, length(cols)), cols))
    }
    vapply(cols, function(cc) {
        as.integer(cc$n_levels %||% NA_integer_)
    }, integer(1L))
}

# columns of a metadata leaf, as a named vector of fingerprints (NA when the
# manifest carried none)
.manifest_colfp <- function(leaf) {
    cols <- leaf$columns
    if (is.null(cols) || length(cols) == 0L || is.character(cols)) {
        return(stats::setNames(character(0), character(0)))
    }
    vapply(cols, function(cc) cc$fingerprint %||% NA_character_, character(1L))
}

# Level counts are opt-in, so a column may carry one on one side only. Compare
# them only where both sides counted; otherwise a manifest with counts against
# one without would report every column as re-levelled.
.manifest_levels_moved <- function(b, a) {
    !is.na(b) && !is.na(a) && !identical(b, a)
}

# Is a level count worth saying out loud?
#
# "leiden_clus (14 levels)" is the whole point of carrying the count;
# "total_expr (1428 levels)" over 1522 cells says only that the column is
# continuous, which the name already told you. The count stays in the manifest
# either way - this governs the sentence, not the data.
.manifest_levels_informative <- function(n, nrow = NULL) {
    if (is.null(n) || is.na(n)) return(FALSE)
    if (n <= 50L) return(TRUE)
    if (is.null(nrow) || is.na(nrow) || nrow <= 0L) return(FALSE)
    n < 0.1 * nrow
}

# rows of the table a metadata leaf describes, for the test above: its cell or
# feature count, read from the summary block
.manifest_leaf_nrow <- function(leaf, path, summary) {
    n <- switch(.manifest_slot_of(path),
        cell_metadata = summary$n_cells[[leaf$keys$spat_unit %||% ""]],
        feat_metadata = summary$n_features[[leaf$keys$feat_type %||% ""]],
        NULL
    )
    if (is.null(n)) NULL else as.integer(n)
}

# "name (N levels)" when the count means something, otherwise just "name"
.manifest_col_label <- function(nm, n, nrow) {
    if (.manifest_levels_informative(n, nrow)) {
        sprintf("%s (%s levels)", nm, n)
    } else {
        nm
    }
}

# fields whose change is worth reporting; everything else is noise
# Keys are not here: records are paired by them, so they never differ.
.MANIFEST_CMP_FIELDS <- c(
    "dtype", "method", "directed", "crs", "compact", "has_unfiltered",
    "overlaps_computed", "on_disk", "fingerprint", "columns", "provenance",
    "n_steps", "step_types"
)

#' @name manifestDiff
#' @title Diff two giotto manifests
#' @description
#' Compare two manifests from [objManifest()] and report what changed, both as
#' data and as a single human-readable sentence. Pure: it reads manifests only,
#' never a `giotto` object.
#'
#' This is what an execution tool attaches to each result so a model, a critic
#' or a scorer knows what an operation actually did, rather than inferring it
#' from printed console output.
#' @param before `gmanifest` or `NULL` (treated as "object did not exist")
#' @param after `gmanifest`
#' @returns list with `changed` (logical), `summary` (character scalar) and
#' `detail` (added / removed / modified / counts / object / unfingerprinted).
#' `changed` reports detected changes only: a record that carries no
#' fingerprint on either side is listed in `unfingerprinted`, and named in
#' `summary`, because it could have changed without the diff seeing it.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' before <- objManifest(g)
#' g <- subsetGiotto(g, cell_ids = head(spatIDs(g), 100))
#' manifestDiff(before, objManifest(g))$summary
#' @export
manifestDiff <- function(before, after) {
    if (is.null(after)) {
        return(list(
            changed = TRUE, summary = "object removed",
            detail = list(object = list(event = "removed"))
        ))
    }
    if (is.null(before)) {
        s <- after$summary
        return(list(
            changed = TRUE,
            summary = sprintf(
                "created (%s cells x %s features)",
                paste(unlist(s$n_cells), collapse = "/"),
                paste(unlist(s$n_features), collapse = "/")
            ),
            detail = list(object = list(event = "created"))
        ))
    }

    fb <- .manifest_flatten(before$slots)
    fa <- .manifest_flatten(after$slots)

    added <- setdiff(names(fa), names(fb))
    removed <- setdiff(names(fb), names(fa))
    shared <- intersect(names(fa), names(fb))

    modified <- list()
    for (p in shared) {
        fields <- character()
        for (f in .MANIFEST_CMP_FIELDS) {
            if (identical(f, "columns")) {
                # compare by column name, so a reordered column list is not
                # reported as a change while an added, dropped, re-levelled or
                # rewritten column is
                cb <- .manifest_colmap(fb[[p]])
                ca <- .manifest_colmap(fa[[p]])
                fb_ <- .manifest_colfp(fb[[p]])
                fa_ <- .manifest_colfp(fa[[p]])
                kept <- intersect(names(cb), names(ca))
                if (!setequal(names(cb), names(ca)) ||
                    any(vapply(kept, function(cc) {
                        .manifest_levels_moved(cb[[cc]], ca[[cc]])
                    }, logical(1L))) ||
                    !identical(fb_[sort(names(fb_))], fa_[sort(names(fa_))])) {
                    fields <- c(fields, f)
                }
                next
            }
            if (!identical(fb[[p]][[f]], fa[[p]][[f]])) fields <- c(fields, f)
        }
        if (length(fields)) {
            modified[[p]] <- list(
                fields = fields,
                before = fb[[p]][fields],
                after = fa[[p]][fields]
            )
        }
    }

    # Records with no fingerprint on either side could have changed without
    # the diff seeing it, so "no state change" must not be claimed for them.
    unfingerprinted <- shared[vapply(shared, function(p) {
        is.null(fb[[p]]$fingerprint) && is.null(fa[[p]]$fingerprint)
    }, logical(1L))]

    phrases <- .manifest_diff_phrases(
        fb, fa, added, removed, modified,
        counts = .manifest_count_changes(before, after),
        summary = after$summary
    )

    obj <- list()
    if (!identical(before$object$uid, after$object$uid)) {
        obj$uid <- list(before = before$object$uid, after = after$object$uid)
        phrases <- c(phrases, "object identity changed")
    }

    detail <- list(
        added = fa[added],
        removed = removed,
        modified = modified,
        counts = .manifest_count_changes(before, after),
        object = obj,
        unfingerprinted = unfingerprinted
    )
    changed <- length(added) > 0L || length(removed) > 0L ||
        length(modified) > 0L || length(obj) > 0L

    n_unfp <- length(unfingerprinted)
    unfp_note <- sprintf("%d %s unfingerprinted", n_unfp,
        if (n_unfp == 1L) "item" else "items")
    summary <- if (length(phrases)) {
        paste(c(phrases, if (n_unfp) unfp_note), collapse = " | ")
    } else if (n_unfp) {
        sprintf("no detected change (%s)", unfp_note)
    } else {
        "no state change"
    }

    list(changed = changed, summary = summary, detail = detail)
}

# Object-level count changes, read from the summary block rather than inferred
# from leaves. A filter or subset moves every object at once; reporting it once
# here is what keeps the rendering compact.
.manifest_count_changes <- function(before, after) {
    out <- list()
    for (f in c("n_cells", "n_features")) {
        b <- before$summary[[f]] %||% list()
        a <- after$summary[[f]] %||% list()
        for (k in union(names(b), names(a))) {
            if (!identical(b[[k]], a[[k]])) {
                out[[paste(f, k, sep = ".")]] <- list(
                    what = f, key = k, before = b[[k]], after = a[[k]]
                )
            }
        }
    }
    out
}

# Build the one-sentence rendering. Grouped by slot so a step that adds one
# matrix reads "expression added: normalized" rather than a path dump.
#
# When the object's cell or feature counts moved, nearly every leaf's content
# moved with them. Enumerating all of those turns one subset into sixteen
# clauses, so the count change is stated once and the leaves whose only change
# is their content are collapsed to a tally. `detail` still carries every leaf.
.manifest_diff_phrases <- function(fb, fa, added, removed, modified,
    counts = list(), summary = list()) {
    phrases <- character()

    for (cc in counts) {
        phrases <- c(phrases, sprintf(
            "%s [%s]: %s -> %s",
            if (identical(cc$what, "n_cells")) "cells" else "features",
            cc$key, cc$before %||% "?", cc$after %||% "?"
        ))
    }
    resized <- 0L

    group <- function(paths) {
        if (!length(paths)) return(list())
        split(paths, vapply(paths, .manifest_slot_of, character(1L)))
    }

    for (grp in names(group(added))) {
        paths <- group(added)[[grp]]
        phrases <- c(phrases, sprintf(
            "%s added: %s", .MANIFEST_SLOT_LABELS[[grp]] %||% grp,
            paste(vapply(paths, function(pp) {
                .manifest_display_name(fa[[pp]], pp)
            }, character(1L)), collapse = ", ")
        ))
    }
    for (grp in names(group(removed))) {
        paths <- group(removed)[[grp]]
        phrases <- c(phrases, sprintf(
            "%s removed: %s", .MANIFEST_SLOT_LABELS[[grp]] %||% grp,
            paste(vapply(paths, function(pp) {
                .manifest_display_name(fb[[pp]], pp)
            }, character(1L)), collapse = ", ")
        ))
    }

    for (p in names(modified)) {
        fields <- modified[[p]]$fields
        label <- .manifest_label(p)

        # Tracks whether this leaf has been accounted for - by a phrase or by
        # the tally. A modified leaf that goes unaccounted reads as
        # "no state change", which is the one thing a diff must never say
        # when something did change.
        accounted <- FALSE

        # metadata columns: name the added columns and their level counts,
        # since that is how a clustering result becomes visible

        if ("columns" %in% fields) {
            cb <- .manifest_colmap(fb[[p]])
            ca <- .manifest_colmap(fa[[p]])
            nrow_a <- .manifest_leaf_nrow(fa[[p]], p, summary)
            new_cols <- setdiff(names(ca), names(cb))
            gone_cols <- setdiff(names(cb), names(ca))
            if (length(new_cols)) {
                phrases <- c(phrases, sprintf(
                    "%s added: %s", label,
                    paste(vapply(new_cols, function(cc) {
                        .manifest_col_label(cc, ca[[cc]], nrow_a)
                    }, character(1L)), collapse = ", ")
                ))
            }
            if (length(gone_cols)) {
                phrases <- c(phrases, sprintf(
                    "%s removed: %s", label,
                    paste(gone_cols, collapse = ", ")
                ))
            }
            # a column that stayed but whose level count or content moved:
            # re-clustering writes into the existing column ("leiden_clus
            # 5 -> 14 levels"), and an in-place edit leaves the count alone
            # and shows in the column's fingerprint only
            kept <- intersect(names(cb), names(ca))
            fpb <- .manifest_colfp(fb[[p]])
            fpa <- .manifest_colfp(fa[[p]])
            relev <- kept[vapply(kept, function(cc) {
                .manifest_levels_moved(cb[[cc]], ca[[cc]]) ||
                    !identical(fpb[cc], fpa[cc])
            }, logical(1L))]
            # A subset changes every column at once. That is the count change
            # already stated, not news about the columns, so it joins the
            # tally instead of listing itself.
            if (length(relev)) {
                if (length(counts)) {
                    resized <- resized + 1L
                } else {
                    phrases <- c(phrases, sprintf(
                        "%s changed: %s", label,
                        paste(vapply(relev, function(cc) {
                            keep <- .manifest_levels_informative(
                                ca[[cc]], nrow_a
                            ) && .manifest_levels_moved(cb[[cc]], ca[[cc]])
                            if (!keep) return(cc)
                            sprintf("%s (%s -> %s levels)", cc,
                                cb[[cc]], ca[[cc]])
                        }, character(1L)), collapse = ", ")
                    ))
                }
            }
            accounted <- length(new_cols) > 0L || length(gone_cols) > 0L ||
                length(relev) > 0L
        }

        # Anything else the leaf changed - most often just the fingerprint,
        # meaning a step overwrote content in place while leaving every name
        # identical. Alongside a count change, a content-only change is the
        # count change again and joins the tally.
        if (!accounted) {
            if (length(counts) && identical(fields, "fingerprint")) {
                resized <- resized + 1L
            } else {
                phrases <- c(phrases, sprintf(
                    "%s %s modified (%s)", label,
                    .manifest_display_name(fa[[p]], p),
                    paste(fields, collapse = ", ")
                ))
            }
        }
    }

    if (resized > 0L) {
        phrases <- c(phrases, sprintf("%d objects changed", resized))
    }

    phrases
}
