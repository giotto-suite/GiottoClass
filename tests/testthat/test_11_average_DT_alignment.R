
# create_average_DT() and create_average_detection_DT() fetch the expression
# matrix and the cell metadata independently. Nothing guarantees the two share
# a cell order, so the grouping has to be keyed on cell_ID rather than assumed
# positional.

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
EX <- getExpression(g, values = "normalized", output = "matrix")
MD <- getCellMetadata(g, output = "data.table")
CLUS <- "leiden_clus"

# reference computed by selecting cells by identifier, never by position
.ref_by_id <- function(fun) {
    lv <- unique(MD[[CLUS]])
    out <- vapply(lv, function(k) {
        ids <- MD[get(CLUS) == k][["cell_ID"]]
        fun(EX[, colnames(EX) %in% ids, drop = FALSE])
    }, numeric(nrow(EX)))
    colnames(out) <- paste0("cluster_", lv)
    out
}

test_that("create_average_DT selects cells by identifier, not position", {
    got <- as.matrix(create_average_DT(g,
        meta_data_name = CLUS, expression_values = "normalized"
    ))
    ref <- .ref_by_id(Matrix::rowMeans)
    expect_equal(got[, colnames(ref)], ref, ignore_attr = TRUE)
})

test_that("create_average_detection_DT selects cells by identifier", {
    got <- as.matrix(create_average_detection_DT(g,
        meta_data_name = CLUS, expression_values = "normalized",
        detection_threshold = 0
    ))
    ref <- .ref_by_id(function(m) Matrix::rowSums(m > 0) / ncol(m))
    expect_equal(got[, colnames(ref)], ref, ignore_attr = TRUE)
})

test_that("results are invariant to cell metadata row order", {
    # The property that actually matters, independent of any one fixture's
    # incidental ordering: permuting the metadata must not change the answer.
    cm <- getCellMetadata(g, output = "cellMetaObj")
    set.seed(9)
    cm[] <- cm[][sample(nrow(cm[]))]
    g2 <- setGiotto(g, cm, verbose = FALSE)

    a <- create_average_DT(g,
        meta_data_name = CLUS, expression_values = "normalized")
    b <- create_average_DT(g2,
        meta_data_name = CLUS, expression_values = "normalized")
    expect_equal(a[, sort(colnames(a))], b[, sort(colnames(b))])

    a <- create_average_detection_DT(g,
        meta_data_name = CLUS, expression_values = "normalized")
    b <- create_average_detection_DT(g2,
        meta_data_name = CLUS, expression_values = "normalized")
    expect_equal(a[, sort(colnames(a))], b[, sort(colnames(b))])
})

test_that("output column order is unchanged by the alignment", {
    # groups are enumerated before the reorder, so callers that index the
    # result positionally are unaffected
    got <- create_average_DT(g,
        meta_data_name = CLUS, expression_values = "normalized")
    expect_equal(colnames(got), paste0("cluster_", unique(MD[[CLUS]])))
})
