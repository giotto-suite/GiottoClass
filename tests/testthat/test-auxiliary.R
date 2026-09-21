# dummy gobject
options("giotto.use_conda" = FALSE)
g <- suppressWarnings(giotto()) # hide no python warning

# dummy expression
m <- matrix(
    seq(9),
    ncol = 3,
    dimnames = list(
        sprintf("gene_%s", letters[seq(3)]),
        sprintf("cell_%d", seq(3))
    )
)

# createMetafeats ####
e <- createExprObj(m,
    name = "test",
    spat_unit = "cell",
    feat_type = "test_feat",
    provenance = "cell"
)
g <- setGiotto(g, e)

num_vec_clus <- c(1, 1, 1, 2, 2, 3)
names(num_vec_clus) <- paste0("gene_", c("a", "b", "c", "a", "c", "b"))

df_clus <- data.frame(
    clus = c(1, 1, 1, 2, 2, 3),
    feat = paste0("gene_", c("a", "b", "c", "a", "c", "b"))
)

df_clus_weight <- data.frame(
    clus = c(1, 1, 1, 2, 2, 3),
    feat = paste0("gene_", c("a", "b", "c", "a", "c", "b")),
    w = c(rep(2, 3), 0.5, 0.1, 1)
)

test_that("createMetafeat can calculate mean values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(rep(c(2, 5, 8), 3), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "mean",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "chara_vec_mean",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g,
        name = "chara_vec_mean",
        output = "data.table"
    )

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)

    g <- createMetafeats(
        g,
        stat = "mean",
        expression_values = "test",
        feat_clusters = df_clus,
        name = "df_mean",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "df_mean", output = "data.table")

    test_m_df <- as.matrix(enr[, 1:3])
    dimnames(test_m_df) <- NULL
    expect_identical(test_m_df, expect_m)
})

test_that("createMetafeat can calculate sum values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(6, 15, 24, 4, 10, 16, 2, 5, 8), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "sum",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "sum",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "sum", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can calculate min values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(1L, 4L, 7L, 1L, 4L, 7L, 2L, 5L, 8L), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "min",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "min",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "min", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_equal(test_m_num, expect_m)
})

test_that("createMetafeat can calculate max values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(3L, 6L, 9L, 3L, 6L, 9L, 2L, 5L, 8L), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "max",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "max",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "max", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_equal(test_m_num, expect_m)
})

test_that("createMetafeat can use weights", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(4, 10, 16, 0.4, 1.3, 2.2, 2, 5, 8), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "mean",
        expression_values = "test",
        feat_clusters = df_clus_weight,
        name = "weighted_means",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g,
        name = "weighted_means",
        output = "data.table"
    )

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can use rescale", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(rep(c(0, 0.5, 1), 3), nrow = 3)

    g <- createMetafeats(
        g,
        stat = "mean",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        rescale_to = c(0, 1),
        name = "scaled_means",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g,
        name = "scaled_means",
        output = "data.table"
    )

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

# annotateGiotto ####

# The mapping is a lookup, not a per-row walk, and an unclustered cell
# (NA in the cluster column) is a cell the clustering never placed rather
# than a cluster with no annotation. See the giottoMulti note in the docs.

.annot_g <- function(clusters) {
    m <- matrix(seq_len(3 * length(clusters)),
        ncol = length(clusters),
        dimnames = list(
            sprintf("gene_%s", letters[seq(3)]),
            sprintf("cell_%d", seq_along(clusters))
        )
    )
    gg <- createGiottoObject(expression = m, verbose = FALSE)
    addCellMetadata(gg,
        new_metadata = stats::setNames(clusters, colnames(m)),
        vector_name = "clus"
    )
}

test_that("annotateGiotto maps cluster values to names", {
    gg <- .annot_g(c(1, 2, 1))
    out <- annotateGiotto(gg,
        annotation_vector = c("1" = "A", "2" = "B"),
        cluster_column = "clus", name = "ct"
    )
    cm <- pDataDT(out)
    expect_identical(cm$ct[match(c("cell_1", "cell_2", "cell_3"), cm$cell_ID)],
        c("A", "B", "A"))
})

test_that("an NA cluster value carries through as NA", {
    gg <- .annot_g(c(1, NA, 2))
    out <- annotateGiotto(gg,
        annotation_vector = c("1" = "A", "2" = "B"),
        cluster_column = "clus", name = "ct"
    )
    cm <- pDataDT(out)
    expect_identical(cm$ct[match(c("cell_1", "cell_2", "cell_3"), cm$cell_ID)],
        c("A", NA, "B"))
})

test_that("an unmapped cluster is reported and becomes NA", {
    gg <- .annot_g(c(1, 2, 3))
    expect_message(
        annotateGiotto(gg,
            annotation_vector = c("1" = "A", "2" = "B"),
            cluster_column = "clus", name = "ct"
        ),
        "no entry in annotation_vector"
    )
    out <- suppressMessages(annotateGiotto(gg,
        annotation_vector = c("1" = "A", "2" = "B"),
        cluster_column = "clus", name = "ct"
    ))
    cm <- pDataDT(out)
    expect_identical(cm$ct[match("cell_3", cm$cell_ID)], NA_character_)
})

test_that("an annotation_vector key matching no cluster is reported", {
    gg <- .annot_g(c(1, 2, 1))
    expect_message(
        annotateGiotto(gg,
            annotation_vector = c("1" = "A", "2" = "B", "9" = "Z"),
            cluster_column = "clus", name = "ct"
        ),
        "do not match any cluster value"
    )
})

test_that("replace = FALSE refines an existing column instead of clobbering", {
    gg <- .annot_g(c(1, 2, 3))
    out <- suppressMessages(annotateGiotto(gg,
        annotation_vector = c("1" = "A", "2" = "B", "3" = "C"),
        cluster_column = "clus", name = "ct"
    ))
    # a second pass that only resolves one cluster
    out2 <- suppressMessages(annotateGiotto(out,
        annotation_vector = c("2" = "B_refined"),
        cluster_column = "clus", name = "ct", replace = FALSE
    ))
    cm <- pDataDT(out2)
    expect_identical(
        cm$ct[match(c("cell_1", "cell_2", "cell_3"), cm$cell_ID)],
        c("A", "B_refined", "C")
    )

    # the default still clobbers: unresolved rows go NA
    out3 <- suppressMessages(annotateGiotto(out,
        annotation_vector = c("2" = "B_refined"),
        cluster_column = "clus", name = "ct"
    ))
    cm3 <- pDataDT(out3)
    expect_identical(
        cm3$ct[match(c("cell_1", "cell_2", "cell_3"), cm3$cell_ID)],
        c(NA, "B_refined", NA)
    )
})

test_that("re-annotating an existing name overwrites it", {
    gg <- .annot_g(c(1, 2, 1))
    out <- annotateGiotto(gg,
        annotation_vector = c("1" = "A", "2" = "B"),
        cluster_column = "clus", name = "ct"
    )
    expect_message(
        annotateGiotto(out,
            annotation_vector = c("1" = "X", "2" = "Y"),
            cluster_column = "clus", name = "ct"
        ),
        "already used"
    )
    out <- suppressMessages(annotateGiotto(out,
        annotation_vector = c("1" = "X", "2" = "Y"),
        cluster_column = "clus", name = "ct"
    ))
    cm <- pDataDT(out)
    expect_identical(cm$ct[match("cell_1", cm$cell_ID)], "X")
    expect_false("ct.1" %in% colnames(cm))
})
