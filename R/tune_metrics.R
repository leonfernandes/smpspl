#' Calculate metrics
#'
#' @param object a tibble of class `smp_spl_results`
#' @param metric_list a list of metrics to be evaluate `.resids`
#' @export
tune_metrics <- function(object, metric_list) {
    if (!inherits(object, "smp_spl_tbl")) stop("Unrecognized `object`.")
    ret <- object |>
        # for each .resids column, calculate listed metrics
        dplyr::mutate(
            .metrics = purrr::map(
                .resids,
                ~ purrr::map(
                    metric_list,
                    \(fn) fn(
                        .x |>
                            dplyr::mutate("estimate" = 0),
                        .resid,
                        estimate
                    )
                )
            )
        )
    # prepend class
    class(ret) <- c("metric_tbl", class(ret))
    ret
}