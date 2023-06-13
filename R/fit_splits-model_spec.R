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
    control = tune::control_resamples()) {
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
    res <- settings_tbl |>
        dplyr::left_join(fitted_tbl, by = dplyr::join_by("analysis_idx")) |>
        subset_resids(".resid", "assessment_idx", metrics)
    class(res) <- c("smp_spl_tbl", class(res))
    res
}