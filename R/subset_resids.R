#' Subsets tail of residuals
#'
#' Helper function to subset a list-column of residuals `resid_col` based on
#' `size_col`. The subset column will have name `.resid`; note that this
#' overwrites any exisiting column of the same name.
#'
#' @param object a tibble.
#' @param resid_col single character string. Column name corresponding to
#'      residuals.
#' @param size_col single character string. Column name corresponding to number
#'      of residuals to be subset from the tail.
#' @inheritParams fit_splits
#' @export
subset_resids <-
    function(object, resid_col, size_col, metrics = NULL) {
        resid_col <- rlang::enquo(resid_col)
        resid_data <-
            object |>
            dplyr::select(!!resid_col)
        size_col <- rlang::enquo(size_col)
        size_data <-
            object |>
            dplyr::pull(!!size_col)
        res_list <-
            seq_along(size_data) |>
            purrr::map(
                ~ vctrs::vec_slice(
                    vctrs::vec_slice(resid_data, .x)[[1]][[1]],
                    vctrs::vec_size(vctrs::vec_slice(resid_data, .x)[[1]][[1]]) -
                        size_data[.x] +
                        1:size_data[.x]
                )
            )
        res <-
            object |>
            dplyr::mutate(.resid = res_list)
        class(res) <- c("resids_tbl", class(res))
        if (!is.null(metrics)) {
            res <- calibrate_metrics(res, metrics)
        }
        return(res)
    }