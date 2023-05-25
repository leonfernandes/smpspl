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

#' @rdname fit_splits
#' @export
fit_splits.model_spec <- function(
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
            obj_fit = list(
                generics::fit(
                    object,
                    formula,
                    vctrs::vec_slice(data, 1:analysis_idx[1])
                )
            ),
            obj_resids = list(
                stats::residuals(
                    obj_fit[[1]]
                )
            )
        )
    res <- settings_tbl |>
        dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
        dplyr::mutate(
            .predictions = purrr::map(
                assessment_idx, ~ vctrs::vec_slice(
                    obj_resids[[1]],
                    vctrs::vec_size(obj_resids[[1]]) - .x + 1:.x
                )
            )
        )
    if (!is.null(metrics)) {
        res <-
            res |>
            dplyr::mutate(
                .metrics = purrr::map(
                    .predictions,
                    ~ metrics(.x, truth = !!rlang::sym(y_nm), estimate = .pred)
                )
            )
    }
    tibble::new_tibble(res, "smp_spl_results")
}

fit_splits_tune <- function(
    object,
    data,
    num_analysis,
    num_assessment,
    ...,
    metrics = NULL,
    control = tune::control_resamples()
) {
    settings_tbl <- sample_splits(data, num_analysis, num_assessment)
    # create resamples and combine
    resamples <-
        settings_tbl |>
        # create manual resamples
        purrr::pmap(
            ~ rsample::make_splits(
                list(analysis = 1:..1, assessment = n - ..2 + 1:..2), data
            )
        ) |>
        # combine resamples
        rsample::manual_rset(ids = settings_tbl$id)
    result <-
        object |>
        # fit manual resamples
        tune::fit_resamples(
            resamples = resamples,
            ...,
            metrics = metrics,
            control = control
        )
    result |>
        dplyr::left_join(settings_tbl, by = dplyr::join_by("id"))
}

fit_splits_impl_workflow <- function(
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
            obj_fit = list(
                generics::fit(
                    object,
                    vctrs::vec_slice(data, 1:analysis_idx[1])
                )
            ),
            obj_resids = list(
                stats::residuals(
                    obj_fit[[1]]
                )
            )
        )
    res <- settings_tbl |>
        dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
        dplyr::mutate(
            .predictions = purrr::map(
                assessment_idx, ~ vctrs::vec_slice(
                    obj_resids[[1]],
                    vctrs::vec_size(obj_resids[[1]]) - .x + 1:.x
                )
            )
        )
    if (!is.null(metrics)) {
        res <-
            res |>
            dplyr::mutate(
                .metrics = purrr::map(
                    .predictions,
                    ~ metrics(.x, truth = !!rlang::sym(y_nm), estimate = .pred)
                )
            )
    }
    tibble::new_tibble(res, "smp_spl_results")
}