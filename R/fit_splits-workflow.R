#' Fit multiple sample splits
#'
#' Fits model according to sample splitting on a grid of values determined by
#' `num_analysis` and `num_assessment`, where each tuple determines the number
#' of analysis and assessment sets.
#'
#'
#' @inheritParams tune::fit_resamples
#' @param data A data frame of predictors and outcomes to use when fitting the
#'  workflow.
#' @param num_analysis A positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment A positive integer specifying number of observations
#'      for goodness-of-fit tests.
#' @param use_fit_resamples  If `TRUE`, uses [tune::fit_resamples()].
#' @param formula An object of class `formula`` (or one that can be coerced to
#'      that class): a symbolic description of the model to be fitted.
#' @export
fit_splits <- function(object, ...) {
    UseMethod("fit_splits")
}

#' @rdname fit_splits
#' @export
fit_splits.workflow <- function(
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

fit_splits_impl_workflow <- function(
    object,
    data,
    num_analysis,
    num_assessment,
    ...,
    metrics = NULL,
    control = tune::control_resamples()) {
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
            .resids = list(
                autoresid::autoresid(
                    .fit[[1]],
                    data,
                    y_nm
                )
            )
        )
    res <- settings_tbl |>
        dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
        dplyr::mutate(
            .resids = purrr::map(
                assessment_idx, ~ vctrs::vec_slice(
                    .resids[[1]],
                    vctrs::vec_size(.resids[[1]]) - .x + 1:.x
                )
            )
        )
    class(res) <- c("smp_spl_tbl", class(res))
    if (!is.null(metrics)) {
        res <- tune_metrics(res, metrics)
    }
    res
}