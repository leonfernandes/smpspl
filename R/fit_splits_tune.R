fit_splits_tune <- function(
    object,
    data,
    num_analysis,
    num_assessment,
    ...,
    metrics = NULL,
    control = tune::control_resamples()) {
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