#' Sample Split Residuals on a Grid
#'
#' Fits model according to sample splitting on a grid of values determined by
#' `num_analysis` and `num_assessment`, where each tuple determines the number
#' of analysis and assessment sets.
#'
#' @inheritParams smpspl
#' @param num_analysis A positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment A positive integer specifying number of observations
#'      for goodness-of-fit tests.
#' @export
#' @examples
#' library(fable)
#' library(tsibble)
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl_grid(data, 2, 2)
smpspl_grid <-
    function(object, data, num_analysis, num_assessment, ...) {
        analysis_idx <- rlang::sym("analysis_idx")
        .fit <- rlang::sym(".fit")
        inn_sym <- rlang::sym("inn")
        .assessment <- rlang::sym(".assessment")
        assessment_idx <- rlang::sym("assessment_idx")
        keep <- rlang::sym("keep")
        n <- vctrs::vec_size(data)
        settings_tbl <-
            # get tibble of sample splitting settings
            sample_splits(
                vctrs::vec_size(data), num_analysis, num_assessment
            )
        estimate_fn <-
            # returns fitted model for aa given number of terms
            function(f_n) {
                try(
                    smpspltools::fit_model(
                        .object = object, .data = vctrs::vec_slice(data, 1:f_n),
                        ... = ...
                    )
                )
            }
        autoresid_fn <-
            # returns full residuals applied based on `data` for a fitted model
            function(fitted_mdl) {
                if (inherits(fitted_mdl, "try-error")) {
                    return(NA)
                }
                fitted_mdl |>
                    rcits::ts2inn(ts = data)
            }
        p <- progressr::progressor(steps = num_analysis * num_assessment)
        nested_mutate <-
            # for each nested .assessment tibble, mutates .subresid tsibble
            function(o) {
                o |>
                    dplyr::mutate(
                        # mutate to replace .assessment column
                        .assessment = purrr::map2(
                            .assessment,
                            inn_sym,
                            ~ dplyr::mutate(
                                # subset inn_sym according to assessment_idx
                                .x,
                                .subresid = purrr::map(
                                    assessment_idx,
                                    \(l_n) {
                                        p()
                                        vctrs::vec_slice(
                                            .y, n - l_n + 1:l_n
                                        )
                                    }
                                )
                            )
                        )
                    )
            }
        ret <-
            settings_tbl |>
            # fit models on `analysis_idx` and calculate residuals on all data
            dplyr::mutate(
                .fit = purrr::map(analysis_idx, estimate_fn),
                keep = purrr::map_lgl(.fit, \(.) !inherits(., "try-error"))
            ) |>
            dplyr::filter(keep) |>
            dplyr::select(-keep) |>
            dplyr::mutate(
                inn_sym = purrr::map(.fit, autoresid_fn)
            ) |>
            dplyr::filter(!inherits(.fit, "try-error")) |>
            # for each sample split get residuals
            nested_mutate() |>
            tibble::new_tibble(class = "smpspl_grid")
        return(ret)
    }