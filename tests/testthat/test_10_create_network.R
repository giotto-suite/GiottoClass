describe("Network Creation Functions", {

    # Test data setup
    g <- test_data$viz

    # Create test matrices for createNetwork
    pca_matrix <- matrix(rnorm(1e4), nrow = 200, ncol = 50)
    rownames(pca_matrix) <- paste0("cell_", 1:200)
    colnames(pca_matrix) <- paste0("PC_", 1:50)

    spatial_coords <- matrix(runif(40, 0, 100), nrow = 20, ncol = 2)
    rownames(spatial_coords) <- paste0("cell_", 1:20)
    colnames(spatial_coords) <- c("x", "y")

    describe("createNetwork()", {

        # createNetwork() ####

        describe("Basic network creation", {

            it("creates sNN network with default parameters", {
                result <- createNetwork(pca_matrix, type = "sNN")

                expect_true(inherits(result, "igraph"))
                expect_true(igraph::is_igraph(result))
                expect_equal(igraph::vcount(result), nrow(pca_matrix))
                expect_true(igraph::is_named(result))
            })

            it("creates kNN network with default parameters", {
                result <- createNetwork(pca_matrix, type = "kNN")

                expect_true(inherits(result, "igraph"))
                expect_true(igraph::is_igraph(result))
                expect_equal(igraph::vcount(result), nrow(pca_matrix))
                expect_true("distance" %in% igraph::edge_attr_names(result))
            })

            it("creates delaunay network with geometry method", {
                result <- createNetwork(
                    spatial_coords,
                    type = "delaunay",
                    method = "geometry"
                )

                expect_true(inherits(result, "igraph"))
                expect_true(igraph::is_igraph(result))
                expect_equal(igraph::vcount(result), nrow(spatial_coords))
            })

        })

        describe("Output format options", {

            it("returns data.table when as.igraph = FALSE", {
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    as.igraph = FALSE
                )

                expect_true(inherits(result, "data.table"))
                expect_true(all(c("from", "to") %in% colnames(result)))
            })

            it("returns igraph when as.igraph = TRUE", {
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    as.igraph = TRUE
                )

                expect_true(inherits(result, "igraph"))
                expect_true(igraph::is_igraph(result))
            })

        })

        describe("Node ID handling", {

            it("uses provided node IDs", {
                custom_ids <- paste0("custom_", 1:nrow(pca_matrix))
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    node_ids = custom_ids
                )

                expect_setequal(igraph::V(result)$name, custom_ids)
            })

            it("uses rownames when node_ids = NULL", {
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    node_ids = NULL
                )

                expect_setequal(igraph::V(result)$name, rownames(pca_matrix))
            })

            it("uses integer indices when no rownames and node_ids = NULL", {
                mat_no_names <- pca_matrix
                rownames(mat_no_names) <- NULL

                result <- createNetwork(
                    mat_no_names,
                    type = "kNN",
                    node_ids = NULL
                )

                expect_setequal(igraph::V(result)$name, as.character(1:nrow(mat_no_names)))
            })

        })

        describe("Network type-specific parameters", {

            it("respects k parameter for kNN networks", {
                k_val <- 5
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    k = k_val,
                    as.igraph = FALSE
                )

                # Each node should have at most k edges (outgoing)
                edge_counts <- result[, .N, by = from]
                expect_true(all(edge_counts$N <= k_val))
            })

            it("respects minimum_shared parameter for sNN networks", {
                min_shared <- 3
                result <- createNetwork(
                    pca_matrix,
                    type = "sNN",
                    minimum_shared = min_shared,
                    as.igraph = FALSE
                )
                expect_true(all(result$shared >= min_shared))
            })

            it("includes weight attribute when include_weight = TRUE", {
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    include_weight = TRUE
                )

                expect_true("weight" %in% igraph::edge_attr_names(result))
                expect_true(all(igraph::E(result)$weight > 0))
            })

            it("includes distance attribute when include_distance = TRUE", {
                result <- createNetwork(
                    pca_matrix,
                    type = "kNN",
                    include_distance = TRUE
                )

                expect_true("distance" %in% igraph::edge_attr_names(result))
                expect_true(all(igraph::E(result)$distance >= 0))
            })

        })

        describe("Delaunay network methods", {

            it("creates delaunay network with RTriangle method", {
                skip_if_not_installed("RTriangle")

                result <- createNetwork(
                    spatial_coords,
                    type = "delaunay",
                    method = "RTriangle"
                )

                expect_true(inherits(result, "igraph"))
                expect_equal(igraph::vcount(result), nrow(spatial_coords))
            })

            it("creates delaunay network with deldir method", {
                skip_if_not_installed("deldir")

                result <- createNetwork(
                    spatial_coords,
                    type = "delaunay",
                    method = "deldir"
                )

                expect_true(inherits(result, "igraph"))
                expect_equal(igraph::vcount(result), nrow(spatial_coords))
            })

        })

        describe("Error handling", {

            it("throws error for invalid network type", {
                expect_error(
                    createNetwork(pca_matrix, type = "invalid"),
                    "should be one of"
                )
            })

            it("throws error for incompatible method and type", {
                expect_error(
                    createNetwork(pca_matrix, type = "sNN", method = "geometry"),
                    "should be"
                )
            })

            it("handles empty input gracefully", {
                empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 5)

                expect_error(
                    createNetwork(empty_matrix, type = "kNN"),
                    "empty matrix"
                )
            })

        })

    })


    describe("createNearestNetwork()", {

        # createNearestNetwork() ####

        describe("Basic nearest network creation", {

            # remove existing
            g@nn_network <- NULL

            it("creates sNN network from PCA reduction", {
                result <- createNearestNetwork(g,
                    type = "sNN",
                    dim_reduction_to_use = "pca",
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]] # there is only one

                expect_true(inherits(result, "giotto"))
                expect_true(inherits(nn, "nnNetObj"))
                expect_equal(nn@nn_type, "sNN")
                expect_true(igraph::is_igraph(nn@network))
            })

            it("creates kNN network from PCA reduction", {
                result <- createNearestNetwork(g,
                    type = "kNN",
                    dim_reduction_to_use = "pca",
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]]

                expect_true(inherits(result, "giotto"))
                expect_true(inherits(nn, "nnNetObj"))
                expect_equal(nn@nn_type, "kNN")
                expect_true(igraph::is_igraph(nn@network))
            })

            it("returns igraph when return_gobject = FALSE", {
                result <- createNearestNetwork(g,
                    type = "sNN",
                    return_gobject = FALSE
                )

                expect_true(inherits(result, "igraph"))
            })

        })

        describe("Parameter validation", {

            g@nn_network <- NULL

            it("respects k parameter", {
                k_val <- 15
                result <- createNearestNetwork(g,
                    type = "kNN",
                    k = k_val,
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]]

                expect_true(inherits(nn, "nnNetObj"))
                out_degrees <- igraph::degree(nn@network, mode = "out")
                expect_true(all(out_degrees <= k_val))
                # more than 80% are = k
                expect_true(mean(out_degrees >= k_val - 1) > 0.8)
                expect_true(all(out_degrees <= k_val))
            })

            it("handles minimum_shared parameter for sNN", {
                min_shared <- 2
                result <- createNearestNetwork(
                    g,
                    type = "sNN",
                    minimum_shared = min_shared,
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]]

                expect_true("shared" %in% igraph::edge_attr_names(nn[]))
                expect_true(all(igraph::E(nn[])$shared >= min_shared))
                expect_true(inherits(nn, "nnNetObj"))
                expect_equal(nn@nn_type, "sNN")
            })

        })

        describe("Network naming", {

            g@nn_network <- NULL

            it("uses default naming when name = NULL", {
                result <- createNearestNetwork(g,
                    type = "kNN",
                    name = NULL,
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]]
                expect_true(objName(nn) == "kNN.pca")
            })

            it("uses custom name when provided", {
                custom_name <- "my_custom_network"
                result <- createNearestNetwork(g,
                    type = "kNN",
                    name = custom_name,
                    return_gobject = TRUE
                )
                nn <- result[["nn_network"]][[1]]
                expect_true(objName(nn) == custom_name)
            })

        })

        describe("Error handling", {

            it("throws error for invalid type", {
                expect_error(
                    createNearestNetwork(g, type = "invalid"),
                    "should be one of"
                )
            })

            it("handles missing dimension reduction gracefully", {
                expect_error(
                    createNearestNetwork(g,
                        dim_reduction_to_use = "nonexistent",
                        return_gobject = FALSE
                    ),
                    "not found"
                )
            })

        })

    })

    describe("createSpatialNetwork()", {

        # createSpatialNetwork() ####

        describe("Basic spatial network creation", {

            it("creates Delaunay spatial network", {
                result <- createSpatialNetwork(g,
                    method = "Delaunay",
                    return_gobject = FALSE
                )

                expect_true(inherits(result, "spatialNetworkObj"))
                expect_equal(objName(result), "Delaunay_network") # default
                expect_equal(result@method, "deldir")
                expect_true(igraph::ecount(result[]) > 0)
                expect_equal(spatUnit(result), activeSpatUnit(g))
            })

            it("creates kNN spatial network", {
                result <- createSpatialNetwork(g,
                    method = "kNN",
                    k = 4,
                    return_gobject = FALSE
                )

                expect_true(inherits(result, "spatialNetworkObj"))
                expect_equal(objName(result), "kNN_network")
                expect_true(igraph::ecount(result[]) > 0)
            })

            it("returns updated giotto object when return_gobject = TRUE", {
                result <- createSpatialNetwork(g,
                    method = "Delaunay",
                    return_gobject = TRUE,
                    verbose = FALSE
                )

                expect_true(inherits(result, "giotto"))
            })

        })

        describe("Output format options", {

            it("returns spatialNetworkObj by default", {
                result <- createSpatialNetwork(
                    g,
                    method = "Delaunay",
                    return_gobject = FALSE,
                    output = "spatialNetworkObj"
                )

                expect_true(inherits(result, "spatialNetworkObj"))
            })

            it("returns data.table when output = 'data.table'", {
                result <- createSpatialNetwork(
                    g,
                    method = "Delaunay",
                    return_gobject = FALSE,
                    output = "data.table"
                )

                expect_true(inherits(result, "data.table"))
                expect_true(all(c("from", "to") %in% colnames(result)))
            })

        })

    })


    # Behaviour locked down for the wrapper rewrite (step 8 of the
    # consolidation). Each test pins a load-bearing behaviour of the
    # legacy createSpatial*Network / createNearestNetwork functions so a
    # regression during the rewrite is caught immediately.
    #
    # Coord columns (sdimx_begin/sdimy_begin/sdimx_end/sdimy_end and the z
    # variants) are intentionally NOT locked here — they're slated for
    # removal in a follow-up so that networks carry only edges and
    # consumers fetch coords from spatLocsObj on demand.
    describe("Wrapper-rewrite lockdown", {

        it("3D Delaunay works with delaunayn_geometry, errors otherwise", {
            # Synthesize a 3D spat_unit by adding sdimz to the existing one
            sl3d <- getSpatialLocations(g, output = "spatLocsObj")
            dt3d <- data.table::copy(sl3d[])
            set.seed(1)
            dt3d[, sdimz := stats::runif(.N, 0, 50)]
            sl3d@coordinates <- dt3d
            g3d <- setSpatialLocations(g, sl3d, verbose = FALSE)

            # geometry method handles 3D
            sn3d <- createSpatialDelaunayNetwork(g3d,
                method = "delaunayn_geometry",
                return_gobject = FALSE, verbose = FALSE
            )
            expect_true(inherits(sn3d, "spatialNetworkObj"))
            expect_true(igraph::ecount(sn3d[]) > 0)

            # non-geometry methods error on 3D
            expect_error(
                createSpatialDelaunayNetwork(g3d,
                    method = "deldir",
                    return_gobject = FALSE, verbose = FALSE
                ),
                "2D"
            )
            expect_error(
                createSpatialDelaunayNetwork(g3d,
                    method = "RTriangle",
                    return_gobject = FALSE, verbose = FALSE
                ),
                "2D"
            )
        })

        it("maximum_distance_delaunay filters long edges", {
            sn_all <- createSpatialNetwork(g,
                method = "Delaunay",
                maximum_distance_delaunay = NULL,
                return_gobject = FALSE, verbose = FALSE
            )
            sn_capped <- createSpatialNetwork(g,
                method = "Delaunay",
                maximum_distance_delaunay = 50,
                return_gobject = FALSE, verbose = FALSE
            )
            expect_lt(igraph::ecount(sn_capped[]), igraph::ecount(sn_all[]))
            expect_true(all(sn_capped[]$distance <= 50))
        })

        it("maximum_distance_delaunay = 'auto' applies a finite filter", {
            sn_all <- createSpatialNetwork(g,
                method = "Delaunay",
                maximum_distance_delaunay = NULL,
                return_gobject = FALSE, verbose = FALSE
            )
            sn_auto <- createSpatialNetwork(g,
                method = "Delaunay",
                maximum_distance_delaunay = "auto",
                return_gobject = FALSE, verbose = FALSE
            )
            # auto should remove at least the outliers
            expect_lte(igraph::ecount(sn_auto[]), igraph::ecount(sn_all[]))
        })

        it("minimum_k preserves more edges than the bare distance filter", {
            sn_no_min <- createSpatialNetwork(g,
                method = "kNN", k = 8,
                maximum_distance_knn = 30,
                minimum_k = 0,
                return_gobject = FALSE, verbose = FALSE
            )
            sn_min2 <- createSpatialNetwork(g,
                method = "kNN", k = 8,
                maximum_distance_knn = 30,
                minimum_k = 2,
                return_gobject = FALSE, verbose = FALSE
            )
            # minimum_k = 2 re-adds nearest neighbours for nodes the
            # distance filter would otherwise have stripped below the
            # floor — so edge count should not decrease
            expect_gte(igraph::ecount(sn_min2[]), igraph::ecount(sn_no_min[]))
        })

        it("delaunay_method = 'delaunayn_geometry' is the geometry backend", {
            sn_geom <- createSpatialNetwork(g,
                method = "Delaunay",
                delaunay_method = "delaunayn_geometry",
                return_gobject = FALSE, verbose = FALSE
            )
            sn_dd <- createSpatialNetwork(g,
                method = "Delaunay",
                delaunay_method = "deldir",
                return_gobject = FALSE, verbose = FALSE
            )
            # Same point set, different backends → same edge set
            edges_undir <- function(x) {
                e <- igraph::as_data_frame(x[], what = "edges")[, c("from", "to")]
                sorted <- t(apply(e, 1, sort))
                sort(unique(paste(sorted[, 1], sorted[, 2], sep = "|")))
            }
            expect_equal(edges_undir(sn_geom), edges_undir(sn_dd))
        })

        it("default network names follow legacy conventions", {
            sn_del <- createSpatialNetwork(g,
                method = "Delaunay", return_gobject = FALSE, verbose = FALSE
            )
            expect_equal(objName(sn_del), "Delaunay_network")

            sn_knn <- createSpatialNetwork(g,
                method = "kNN", k = 4,
                return_gobject = FALSE, verbose = FALSE
            )
            expect_equal(objName(sn_knn), "kNN_network")

            g_knn <- createNearestNetwork(g,
                type = "kNN", dim_reduction_to_use = "pca",
                return_gobject = TRUE
            )
            nn_knn <- getNearestNetwork(g_knn,
                nn_type = "kNN", name = "kNN.pca", output = "nnNetObj"
            )
            expect_equal(objName(nn_knn), "kNN.pca")

            g_snn <- createNearestNetwork(g,
                type = "sNN", dim_reduction_to_use = "pca",
                return_gobject = TRUE
            )
            nn_snn <- getNearestNetwork(g_snn,
                nn_type = "sNN", name = "sNN.pca", output = "nnNetObj"
            )
            expect_equal(objName(nn_snn), "sNN.pca")
        })

        it("createNearestNetwork preserves dimObj provenance", {
            g@nn_network <- NULL
            dim_obj <- getDimReduction(g,
                reduction = "cells", reduction_method = "pca",
                name = "pca", output = "dimObj"
            )
            res <- createNearestNetwork(g,
                type = "kNN", dim_reduction_to_use = "pca",
                return_gobject = TRUE
            )
            nn <- res[["nn_network"]][[1]]
            expect_equal(prov(nn), prov(dim_obj))
        })

    })


    describe("radius networks", {

        it("createNetwork builds a radius network end to end", {
            set.seed(8)
            x <- matrix(runif(300 * 2, 0, 100), ncol = 2)
            rownames(x) <- sprintf("c%03d", seq_len(300))

            g <- createNetwork(x, radiusNetworkParam(eps = 14, output = "igraph"))
            expect_s3_class(g, "igraph")
            expect_false(igraph::is_directed(g))

            dt <- createNetwork(x, radiusNetworkParam(eps = 14, output = "data.table"))
            expect_s3_class(dt, "data.table")
            expect_true(all(c("from", "to", "weight", "distance") %in% names(dt)))
            expect_equal(igraph::ecount(g), nrow(dt))
        })

        it("a radius network has density-following degree, unlike kNN", {
            # the reason to have it: one dense cluster and one sparse, same eps
            set.seed(12)
            dense <- cbind(rnorm(200, 0, 1), rnorm(200, 0, 1))
            sparse <- cbind(rnorm(200, 100, 12), rnorm(200, 100, 12))
            x <- rbind(dense, sparse)

            dt <- GiottoClass:::.net_dt_radius(x, eps = 2)
            deg <- table(factor(c(dt$from, dt$to), levels = seq_len(400)))
            expect_gt(mean(deg[1:200]), mean(deg[201:400]))

            # kNN would give every node the same degree by construction
            knn <- GiottoClass:::.net_dt_knn(x, k = 6L)
            kdeg <- table(factor(knn$from, levels = seq_len(400)))
            expect_true(all(kdeg == 6L))
        })

        it("radiusNetworkParam on a giotto defaults to spatial coordinates", {
            rlang::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

            set.seed(3)
            n <- 200L
            locs <- data.table::data.table(
                cell_ID = sprintf("c%03d", seq_len(n)),
                sdimx = runif(n, 0, 1000), sdimy = runif(n, 0, 1000)
            )
            m <- matrix(rpois(6 * n, 5), nrow = 6L,
                        dimnames = list(paste0("g", 1:6), locs$cell_ID))
            g <- createGiottoObject(expression = m, spatial_locs = locs)
            emb <- matrix(rnorm(n * 5, sd = 2), nrow = n,
                          dimnames = list(locs$cell_ID, NULL))
            g <- setDimReduction(g, create_dim_obj(
                coordinates = emb, name = "pca", reduction_method = "pca",
                spat_unit = "cell", feat_type = "rna"
            ))

            # a dedicated method exists rather than inheriting the NN one
            expect_identical(
                selectMethod("createNetwork", c("giotto", "radiusNetworkParam"))@defined[[2L]],
                "radiusNetworkParam"
            )

            spatial <- createNetwork(g,
                radiusNetworkParam(eps = 60, output = "data.table")
            )
            # every edge is within eps of the *spatial* coordinates
            expect_lte(max(spatial$distance), 60)
            ref <- as.matrix(stats::dist(as.matrix(locs[, .(sdimx, sdimy)])))
            expect_equal(nrow(spatial), sum(ref > 0 & ref <= 60) / 2L)

            # the PCA-space behaviour is still reachable, but only on request
            expr <- createNetwork(g,
                radiusNetworkParam(eps = 3, output = "data.table"),
                space = "expression"
            )
            ref_pca <- as.matrix(stats::dist(emb))
            expect_equal(nrow(expr), sum(ref_pca > 0 & ref_pca <= 3) / 2L)
        })

        it("createSpatialNetwork gives radiusNetworkParam a user path", {
            rlang::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

            set.seed(3)
            n <- 200L
            locs <- data.table::data.table(
                cell_ID = sprintf("c%03d", seq_len(n)),
                sdimx = runif(n, 0, 1000), sdimy = runif(n, 0, 1000)
            )
            m <- matrix(rpois(6 * n, 5), nrow = 6L,
                        dimnames = list(paste0("g", 1:6), locs$cell_ID))
            g <- createGiottoObject(expression = m, spatial_locs = locs)

            g2 <- createSpatialNetwork(g, method = "radius", radius = 60)
            expect_true("radius_network" %in%
                list_spatial_networks_names(g2, spat_unit = "cell"))

            sn <- getSpatialNetwork(g2, name = "radius_network")
            expect_identical(sn@method, "radius")
            expect_identical(sn@parameters$eps, 60)

            dt <- createSpatialNetwork(g, method = "radius", radius = 60,
                return_gobject = FALSE, output = "data.table")
            expect_s3_class(dt, "data.table")
            expect_equal(nrow(dt), igraph::ecount(sn[]))
            expect_lte(max(dt$distance), 60)

            # the cutoff is not optional for this method
            expect_error(createSpatialNetwork(g, method = "radius"), "radius")

            # and the two established methods are untouched
            expect_identical(
                nrow(createSpatialNetwork(g, method = "Delaunay",
                    return_gobject = FALSE, output = "data.table")),
                558L
            )
            expect_identical(
                nrow(createSpatialNetwork(g, method = "kNN", k = 4,
                    return_gobject = FALSE, output = "data.table")),
                800L
            )
        })

    })

})