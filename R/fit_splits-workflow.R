#' @rdname fit_splits
#' @export
fit_splits.workflow <-
    function(
        object,
        data,
        num_analysis,
        num_assessment,
        use_fit_resamples = FALSE,
        ...,
        metrics = NULL,
        control = tune::control_resamples()
    ) {
        fn <- fit_splits_impl_workflow
        if (use_fit_resamples) {
            fn <- fit_splits_tune
        }
        fn(
            object = object,
            data = data,
            num_analysis = num_analysis,
            num_assessment = num_assessment,
            metrics = metrics,
            control = control
        )
    }

fit_splits_impl_workflow <-
    function(
        object,
        data,
        num_analysis,
        num_assessment,
        ...,
        metrics = NULL,
        control = tune::control_resamples()
    ) {
        y_nm <- tune::outcome_names(object)
        settings_tbl <- sample_splits(data, num_analysis, num_assessment)
        fitted_tbl <-
            settings_tbl |>
            dplyr::group_by(analysis_idx) |>
            dplyr::summarise(
                .fit = list(
                    generics::fit(
                        object,
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
        res <- settings_tbl |>
            dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
            subset_resids(".resid", "assessment_idx", metrics)
        class(res) <- c("smp_spl_tbl", class(res))
        res
    }