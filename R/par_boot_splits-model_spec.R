#' @rdname par_boot_splits
#' @param formula An object of class `formula` (or one that can be coerced to
#'      that class): a symbolic description of the model to be fitted.
#' @export
par_boot_splits.model_spec <-
    function(
        object,
        formula,
        data,
        num_analysis,
        num_resamples,
        resample_size,
        quantiles,
        burn_in = 200L,
        ...,
        metrics,
        control
    ) {
        # fit full model
        fitted_model <-
            generics::fit(object = object, formula = formula, data = data)
        # calculate residuals
        fitted_residuals <-
            fitted_model |>
            autoresid::autoresid(
                new_data = data,
                outcome = tune::outcome_names(formula)
            )
        bootstrapped_metrics <-
            purrr::map(
                1:num_resamples,
                function(.) {
                    new_resids <-
                        sample(
                            fitted_residuals$.resid,
                            resample_size + burn_in,
                            replace = TRUE
                        )
                    new_data <-
                        fitted_model |>
                        # simulate new data
                        simts::simts(resample_size, new_resids)
                    # refit model on new_data and calculate metrics
                    ret <-
                        fit_splits(
                            object = object,
                            formula = formula,
                            data = new_data,
                            num_analysis = num_analysis,
                            num_assessment = 1L,
                            ... = ...,
                            metrics = metrics,
                            control = control
                        ) |>
                        dplyr::select(analysis_idx, .metric) |>
                        tidyr::unnest(".metric")
                    ret
                }
            ) |>
            purrr::list_rbind(names_to = "boot_id")
        bootstrapped_metrics |>
            # for each setting, calculate respective quantiles
            dplyr::group_by(analysis_idx, .metric, lag) |>
            dplyr::reframe(
                quantile_df(.estimate, probs = quantiles)
            )
    }