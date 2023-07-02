#' @rdname fit_splits
#' @export
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(modeltime)
#' data <- data.frame(x = rnorm(10), date = Sys.Date() + 0:9)
#' # Consider a arima model
#' arima_spec <-
#'      arima_reg() |>
#'      set_engine("arima")
#' fit_splits(arima_spec, x ~ date, data, 2, 2)
fit_splits.model_spec <-
    function(
        object,
        formula,
        data,
        num_analysis,
        num_assessment,
        ...,
        metrics = NULL,
        control = tune::control_resamples()
    ) {
        y_nm <- tune::outcome_names(formula)
        settings_tbl <- sample_splits(data, num_analysis, num_assessment)
        fitted_tbl <-
            settings_tbl |>
            dplyr::group_by(analysis_idx) |>
            dplyr::summarise(
                .fit = list(
                    generics::fit(
                        object,
                        formula,
                        vctrs::vec_slice(data, 1:analysis_idx[1])
                    )
                ),
                .resid = list(
                    autoresid::autoresid(
                        .fit[[1]],
                        data,
                        y_nm
                    )
                )
            )
        res <-
            settings_tbl |>
            dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
            subset_resids(".resid", "assessment_idx", metrics)
        class(res) <- c("smp_spl_tbl", class(res))
        res
    }
