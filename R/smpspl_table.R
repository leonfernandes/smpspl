#' Sample Split Residuals for a List of Models
#'
#' @inheritParams smpspl
#' @param model_list list of models to be fit
#' @export
smpspl_table <-
    function(model_list, data, f_n, l_n, ...) {
        ret <-
            model_list |>
            purrr::map(
                \(obj) smpspl(
                    object = obj,
                    data = data,
                    f_n = f_n,
                    l_n = l_n,
                    ...
                )
            ) |>
            purrr::list_rbind(names_to = "model_id")
        class(ret) <- c("smpspl_table", class(ret))
        ret
    }

#' Sample Split Residuals on a Grid for a List of Models
#'
#' @inheritParams smpspl_grid
#' @param model_list list of models to be fit
#' @export
smpspl_table_grid <-
    function(
        model_list,
        data,
        num_analysis,
        num_assessment,
        ...
    ) {
        ret <-
            model_list |>
            purrr::map(
                \(obj) smpspl_grid(
                    object = obj,
                    data = data,
                    num_analysis = num_analysis,
                    num_assessment = num_assessment,
                    ...
                )
            ) |>
            purrr::list_rbind(names_to = "model_id")
        class(ret) <- c("smpspl_table_nested", class(ret))
        ret
    }