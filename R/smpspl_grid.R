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
#' library(smpspltools)
#' library(generics)
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl_grid(data, 2, 2)
#' # Calculate lanyard features
#' my_acf <-
#'      function(x) {
#'          data.frame(t = x, e = 0) |>
#'              lanyard::acf_metric(t, e) |>
#'              tidy()
#'      }
#' o |>
#'      features(.resid, .subresid, .assessment, features = my_acf) |>
#'      tidyr::unnest(.nested_features) |>
#'      tidyr::unnest(.features)
#' library(dplyr)
#' library(parsnip)
#' library(modeltime)
#' # Repeat with `modeltime::arima_reg`
#' arima_spec <-
#'      arima_reg() |>
#'      set_engine("arima")
#' o2 <- smpspl_grid(arima_spec, data, 2, 2, formula = x ~ date)
#' o2 |>
#'      features(.resid, .subresid, .assessment, features = my_acf) |>
#'      tidyr::unnest(.nested_features) |>
#'      tidyr::unnest(.features)
smpspl_grid <-
    function(object, data, num_analysis, num_assessment, ...) {
        analysis_idx <- rlang::sym("analysis_idx")
        .fit <- rlang::sym(".fit")
        .resid <- rlang::sym(".resid")
        .assessment <- rlang::sym(".assessment")
        assessment_idx <- rlang::sym("assessment_idx")
        n <- vctrs::vec_size(data)
        settings_tbl <-
            # get tibble of sample splitting settings
            sample_splits(
                vctrs::vec_size(data), num_analysis, num_assessment
            )
        estimate_fn <-
            # returns fitted model for aa given number of terms
            function(f_n) {
                smpspltools::fit_model(
                    .object = object, .data = vctrs::vec_slice(data, 1:f_n),
                    ... = ...
                )
            }
        .outcome <- smpspltools::extract_outcome(object, ...)
        autoresid_fn <-
            # returns full residuals applied based on `data` for a fitted model
            function(fitted_mdl) {
                fitted_mdl |>
                    autoresid::autoresid(new_data = data, outcome = .outcome)
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
                            .resid,
                            ~ dplyr::mutate(
                                # subset .resid according to assessment_idx
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
                .resid = purrr::map(.fit, autoresid_fn)
            ) |>
            # for each sample split get residuals
            nested_mutate() |>
            tibble::new_tibble(class = "smpspl_grid")
        return(ret)
    }