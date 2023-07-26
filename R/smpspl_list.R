#' Sample Split Wrapper for a List of Models
#'
#' Convenient wrapper for iterating over a list of models. The first parameter
#' of `fun` should take a registered model to be fit to the data.
#' @param model_list list of models to be fit
#' @param fun a `smpspl` function to be applied
#' @param ... parameters other than "object" of `fun`
#' @export
smpspl_list <-
    function(model_list, fun, ...) {
        dot_args <- list(...)
        my_fun <-
            function(.x) {
                do.call(fun, append(list(object = .x), dot_args))
            }
        ret <-
            model_list |>
            purrr::map(my_fun) |>
            purrr::list_rbind(names_to = "model_id")
        class(ret) <- c("smpspl_table", class(ret))
        ret
    }