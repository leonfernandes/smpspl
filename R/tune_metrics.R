#' Calculate metrics
#'
#' Assumes each row of `.resid` list-column is a tibble with also contains a
#' column `.resid`.
#'
#' @param object a tibble of class `smp_spl_results`
#' @param metric_list a list of metrics to evaluate independence of `.resid`
#' @export
tune_metrics <- function(object, metric_list) {
    if (!inherits(object, "resids_tbl")) stop("Unrecognized `object`.")
    ret <-
        object |>
        # for each .resid column, calculate listed metrics
        dplyr::mutate(
            .metrics = purrr::map(
                .resid,
                ~ purrr::map(
                    metric_list,
                    \(fn) fn(
                        .x |>
                            dplyr::mutate("estimate" = 0),
                        ".resid",
                        "estimate"
                    )
                )
            )
        )
    # prepend class
    class(ret) <- c("metric_tbl", class(ret))
    ret
}