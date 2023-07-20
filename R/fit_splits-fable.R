#' Fit multiple sample splits to fable models
#'
#' @rdname fit_splits_fable
#' @inheritParams fabletools::estimate
#' @param num_analysis A positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment A positive integer specifying number of observations
#'      for goodness-of-fit tests.
#' @export
#' @examples
#' library(fable)
#' data <-
#'      tsibble::tsibble(
#'          value = rnorm(100), date = Sys.Date() + 0:99, index = date
#'      )
#' # Consider an AR(1) model
#' data |>
#'      fit_splits_tbl_ts(
#'          .model = ARIMA(value ~ pdq(1, 0, 0) + PDQ(0, 0, 0)), 2, 2
#'      )
fit_splits_tbl_ts <-
    function(
        .data,
        .model,
        num_analysis,
        num_assessment,
        ...
    ) {
        analysis_idx <- rlang::sym("analysis_idx")
        .fit <- rlang::sym(".fit")
        .resid <- rlang::sym(".resid")
        num_sim <- vctrs::vec_size(.data)
        settings_tbl <-
            sample_splits_tbl_ts(
                num_sim, num_analysis, num_assessment
            )
        estimate_fn <-
            function(f_n) {
                .data |>
                    vctrs::vec_slice(
                        1:f_n
                    ) |>
                    fabletools::estimate(
                        .model = .model,
                        ... = ...
                    )
        }
        autoresid_fn <-
            function(fitted_mdl) {
                # extracts all fitted residuals from a model
                fitted_mdl |>
                    autoresid::autoresid(new_data = .data)
            }
        nested_mutate_tbl_ts <-
            function(.data) {
                .assessment <- rlang::sym(".assessment")
                .data |>
                    dplyr::mutate(
                        .assessment = purrr::map2(
                            .assessment,
                            .resid,
                            ~ dplyr::mutate(
                                .x,
                                .subresid = purrr::map(
                                    assessment_idx,
                                    \(ln) vctrs::vec_slice(
                                        .y, num_sim - ln + 1:ln
                                    )
                                )
                            )
                        )
                    )
            }
        res <-
            settings_tbl |>
            dplyr::mutate(
                .fit = purrr::map(analysis_idx, estimate_fn),
                .resid = purrr::map(.fit, autoresid_fn)
            ) |>
            nested_mutate_tbl_ts() |>
            dplyr::select(-c(.fit, .resid))
        class(res) <- c("smp_spl_tbl", class(res))
        res
    }

sample_splits_tbl_ts <-
    function(n, num_analysis, num_assessment) {
        analysis_idx <- seq(
            n / num_analysis, n,
            length.out = num_analysis
        ) |>
            as.integer()
        assessment_idx <- seq(
            n, n / num_assessment,
            length.out = num_assessment
        ) |>
            as.integer()
        settings_tbl <-
            # combine and save all sample split settings
            tidyr::expand_grid(
                analysis_idx = analysis_idx,
                assessment_idx = assessment_idx
            ) |>
            tidyr::nest(.assessment = assessment_idx)
        tibble::new_tibble(settings_tbl, "smp_spl_nest")
    }