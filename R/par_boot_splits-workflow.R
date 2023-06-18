#' @rdname par_boot_splits
par_boot_splits.workflow <-
    function(
        object,
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
        fitted_wfl <-
            generics::fit(object = object, data = data)
        # calculate residuals
        fitted_residuals <-
            fitted_wfl |>
            hardhat::extract_fit_engine() |>
            autoresid::autoresid(
                new_data = data,
                outcome = tune::outcome_names(object)
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
                        fitted_wfl |>
                        hardhat::extract_fit_engine() |>
                        # simulate new data
                        simts::simts(resample_size, new_resids)
                    # refit model on new_data and calculate metrics
                    fit_splits(
                        object = object,
                        data = new_data,
                        num_analysis = num_analysis,
                        num_assessment = 1L,
                        ... = ...,
                        metrics = metrics,
                        control = control
                    ) |>
                    dplyr::select(analysis_idx, .metrics) |>
                    dplyr::mutate(
                        .metrics = purrr::map(.metrics, ~ list_cbind(.x))
                    )
                }
            ) |>
            purrr::list_rbind(names_to = "boot_id")
        bootstrapped_metrics |>
            # add list-column of combined metrics
            dplyr::mutate(
                comb_met = purrr::map(
                    .metrics,
                    ~ purrr::reduce(
                        # join metrics by lag
                        .x,
                        \(a, b) dplyr::inner_join(
                            a, b, by = dplyr::join_by("lag")
                        )
                    ) |>
                    # pivot metrics and form long tibble
                    tidyr::pivot_longer(
                        cols = -lag,
                        names_to = "metric"
                    ) |>
                    # encode metric as factor
                    dplyr::mutate(
                        metric = factor(metric)
                    )
                )
            ) |>
            # for each analysis_idx, convert list-columns to columns
            dplyr::group_by(analysis_idx) |>
            dplyr::reframe(long_metric = purrr::list_rbind(comb_met)) |>
            tidyr::unnest(long_metric) |>
            # for each setting, calculate respective quantiles
            dplyr::group_by(analysis_idx, lag, metric) |>
            dplyr::reframe(
                value = quantile_df(value, probs = quantiles)
            ) |>
            tidyr::unnest()
    }