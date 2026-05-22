# Global options for testing
options("giotto.use_conda" = FALSE)
options("lifecycle_verbosity" = "quiet")
options("giotto.no_python_warn" = TRUE)

# Test Data Setup
# ---------------
setup_test_data <- function() {
    # Load test objects
    viz <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
    activeSpatUnit(viz) <- "aggregate"

    # load example data
    ex <- GiottoData::loadSubObjectMini("exprObj")
    sl <- GiottoData::loadSubObjectMini("spatLocsObj")
    cm <- GiottoData::loadSubObjectMini("cellMetaObj")
    fm <- GiottoData::loadSubObjectMini("featMetaObj")
    # network subobject minis predate the 0.6.0 igraph migration; run
    # initialize() so the in-class migration step normalizes them.
    sn <- methods::initialize(
        GiottoData::loadSubObjectMini("spatialNetworkObj")
    )
    enr <- GiottoData::loadSubObjectMini("spatEnrObj")
    nn <- methods::initialize(
        GiottoData::loadSubObjectMini("nnNetObj")
    )
    dr <- GiottoData::loadSubObjectMini("dimObj")
    gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
    gpoints <- GiottoData::loadSubObjectMini("giottoPoints")

    list(
        ex = ex, sl = sl, cm = cm, fm = fm, sn = sn,
        enr = enr, nn = nn, dr = dr, gpoly = gpoly,
        gpoints = gpoints, viz = viz
    )
}

test_data <- setup_test_data()
set.seed(1234) # reproducibility seed
