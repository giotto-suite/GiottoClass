# Run on library loading


.onAttach <- function(libname, pkgname) {
    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoClass")
        options("giotto.check_version" = FALSE)
    }

    init_option("giotto.py_path", NULL)
    init_option("giotto.init", TRUE)
    init_option("giotto.check_valid", TRUE)
    init_option("giotto.plotengine3d", "plotly")
    init_option("giotto.update_param", TRUE)
    init_option("giotto.no_python_warn", FALSE)
    init_option("giotto.init_check_severity", "stop")
    init_option("giotto.overlap_point_method", "vector")
}

.onLoad <- function(libname, pkgname) {
    all_matrix <- c("matrix", "Matrix")
    update_matrix_sig <- FALSE

    # suppress known warnings about onload class and method extensions
    suppressWarnings({
        # extensible classunions --------------------------------------------#
        # Only the package that defines a union may add another package's
        # classes to it, so this hook has to live with `allMatrix`.
        if (requireNamespace("DelayedArray", quietly = TRUE)) {
            getClass("DelayedArray")
            all_matrix <- c(all_matrix, "DelayedArray")
            update_matrix_sig <- TRUE
            # A DelayedArray subclass only dispatches on `allMatrix` if its
            # package was loaded when the union was last set, even though
            # `is()` says TRUE either way. `scale_flex()` produces
            # `ScaledMatrix` (what `expression_values = "scaled"` holds), so
            # load it here; otherwise scaled expression reaches no method.
            requireNamespace("ScaledMatrix", quietly = TRUE)
        }
        if (requireNamespace("BPCells", quietly = TRUE)) {
            getClass("IterableMatrix")
            all_matrix <- c(all_matrix, "IterableMatrix")
            update_matrix_sig <- TRUE
        }
        # signature update
        if (isTRUE(update_matrix_sig)) {
            setClassUnion("allMatrix", members = all_matrix)
        }
    })
}
