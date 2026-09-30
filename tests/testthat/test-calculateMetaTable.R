# calculateMetaTable() now takes its groups from cell metadata via
# spatValues(slot = "cell_metadata") and its means from one grouped featStats
# call. The output contract -- columns, group order, feature order -- is the
# per-group loop it replaced, kept here as the reference.

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

# the previous implementation, verbatim apart from the function name
.old_meta_table <- function(gobject, expression_values = "normalized",
    metadata_cols, selected_feats = NULL) {
    uniq_ID <- NULL
    metadata <- data.table::copy(pDataDT(gobject))
    if (length(metadata_cols) > 1) {
        metadata[, uniq_ID := paste(.SD, collapse = "-"),
            by = seq_len(nrow(metadata)), .SDcols = metadata_cols
        ]
    } else {
        metadata[, uniq_ID := get(metadata_cols)]
    }
    possible_groups <- unique(metadata[, metadata_cols, with = FALSE])
    if (length(metadata_cols) > 1) {
        possible_groups[, uniq_ID := paste(.SD, collapse = "-"),
            by = seq_len(nrow(possible_groups)), .SDcols = metadata_cols
        ]
    } else {
        possible_groups[, uniq_ID := get(metadata_cols)]
    }
    expr_values <- getExpression(gobject,
        values = expression_values, output = "matrix"
    )
    if (!is.null(selected_feats)) {
        expr_values <- expr_values[rownames(expr_values) %in% selected_feats, ]
    }
    result_list <- list()
    for (row in seq_len(nrow(possible_groups))) {
        uniq_identifiier <- possible_groups[row][["uniq_ID"]]
        selected_cell_IDs <- metadata[uniq_ID == uniq_identifiier][["cell_ID"]]
        sub_expr_values <- expr_values[
            ,
            colnames(expr_values) %in% selected_cell_IDs
        ]
        if (is.vector(sub_expr_values) == FALSE) {
            subvec <- rowMeans_flex(sub_expr_values)
        } else {
            subvec <- sub_expr_values
        }
        result_list[[row]] <- subvec
    }
    finaldt <- data.table::as.data.table(do.call("rbind", result_list))
    possible_groups_res <- cbind(possible_groups, finaldt)
    data.table::melt.data.table(
        possible_groups_res,
        id.vars = c(metadata_cols, "uniq_ID")
    )
}

test_that("one grouping column matches the per-group loop exactly", {
    got <- calculateMetaTable(g, metadata_cols = "leiden_clus")
    ref <- .old_meta_table(g, metadata_cols = "leiden_clus")
    expect_identical(got, ref)
})

test_that("two grouping columns match, combined label included", {
    cols <- c("leiden_clus", "custom_leiden")
    got <- calculateMetaTable(g, metadata_cols = cols)
    ref <- .old_meta_table(g, metadata_cols = cols)
    expect_identical(got, ref)
})

test_that("selected_feats and other expression values match", {
    feats <- head(featIDs(g), 10)
    got <- calculateMetaTable(g,
        expression_values = "scaled",
        metadata_cols = "leiden_clus", selected_feats = feats
    )
    ref <- .old_meta_table(g,
        expression_values = "scaled",
        metadata_cols = "leiden_clus", selected_feats = feats
    )
    expect_identical(got, ref)
})

test_that("a single selected feature works (it dropped to a vector before)", {
    f <- featIDs(g)[1]
    expect_error(.old_meta_table(g,
        metadata_cols = "leiden_clus", selected_feats = f
    ))
    got <- calculateMetaTable(g,
        metadata_cols = "leiden_clus", selected_feats = f
    )
    ref <- calculateMetaTable(g, metadata_cols = "leiden_clus")
    ref <- ref[variable == f]
    ref[, variable := droplevels(variable)]
    expect_identical(got, ref)
})

test_that("groups come from cell metadata even when a feature shares the name", {
    f <- featIDs(g)[1]
    g2 <- addCellMetadata(g,
        new_metadata = data.frame(cell_ID = spatIDs(g), x = "a"),
        by_column = TRUE, column_cell_ID = "cell_ID"
    )
    # rename the new column to collide with a feature
    cx <- getCellMetadata(g2, output = "cellMetaObj")
    data.table::setnames(cx[], "x", f)
    g2 <- setCellMetadata(g2, cx, verbose = FALSE, initialize = FALSE)

    got <- calculateMetaTable(g2, metadata_cols = f)
    expect_identical(unique(as.character(got$uniq_ID)), "a")
})

test_that("missing metadata columns error naming them", {
    expect_error(
        calculateMetaTable(g, metadata_cols = c("leiden_clus", "nope")),
        "not found in cell metadata: nope"
    )
})


# spatValues(slot = ) ------------------------------------------------------ #

test_that("slot scopes spatValues to one slot", {
    f <- featIDs(g)[1]
    cx <- getCellMetadata(g, output = "cellMetaObj")
    cx[][, (f) := "meta"]
    g2 <- setCellMetadata(g, cx, verbose = FALSE, initialize = FALSE)

    # unscoped: expression is searched first
    expect_type(spatValues(g2, feats = f)[[f]], "double")
    # scoped: cell metadata
    expect_identical(
        unique(spatValues(g2, feats = f, slot = "cell_metadata")[[f]]),
        "meta"
    )
})

test_that("slot refuses a name param that points elsewhere, and bad values", {
    expect_error(
        spatValues(g, feats = "leiden_clus", slot = "cell_metadata",
            expression_values = "raw"),
        "conflicts with `expression_values`"
    )
    expect_error(spatValues(g, feats = "leiden_clus", slot = "metadata"))
    # agreeing params are fine
    expect_no_error(spatValues(g, feats = featIDs(g)[1],
        slot = "expression", expression_values = "raw"))
})

test_that("svkey forwards slot", {
    k <- svkey("leiden_clus", slot = "cell_metadata")
    expect_identical(k@slot, "cell_metadata")
    expect_identical(
        spatValues(g, svkey = k),
        spatValues(g, feats = "leiden_clus", slot = "cell_metadata")
    )
})
