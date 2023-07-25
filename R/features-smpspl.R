#' Features for sample splitting
#'
#' @inheritParams fabletools::features
#' @param .var The variable to compute features on.
#' @rdname features_smpspl
#' @exportS3Method fabletools::features
features.smpspl_grid <-
    function(.tbl, .var, features, ...) {
        .var <- rlang::enquo(.var)
        .tbl |>
            dplyr::mutate(
                .assessment = purrr::map(
                    !!.var,
                    ~ .x |>
                        dplyr::mutate(
                            .features = purrr::map(
                                .subresid,
                                function(.) {
                                    . |>
                                        fabletools::features(
                                            .var = .resid,
                                            features = features
                                        )
                                }
                            )
                        ) |>
                        dplyr::select(-.subresid)
                )
            ) |>
            tibble::new_tibble(class = "smpspl_nested_features")
    }
