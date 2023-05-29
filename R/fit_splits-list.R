#' Fit multiple sample splits from a list of models
#'
#' @inheritParams fit_splits
#' @param object list of `model_spec` and/or `workflow` object to be fit
#' @export
fit_splits_list <-
    function(
        object,
        data,
        formula = NULL,
        num_analysis,
        num_assessment,
        use_fit_resamples = FALSE,
        ...,
        metrics = NULL,
        control = tune::control_resamples()
    ) {
        ret <-
            object |>
            purrr::map(\(obj) fit_splits(
                object = obj,
                data = data,
                formula = formula,
                num_analysis = num_analysis,
                num_assessment = num_assessment,
                use_fit_resamples = use_fit_resamples,
                ... = ...,
                metrics = metrics,
                control = control
            )) |>
            purrr::list_rbind(names_to = "model_id")
        class(ret) <- c("list_smp_spl_tbl", class(ret))
        ret
    }