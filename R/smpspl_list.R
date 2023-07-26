#' Sample Split Wrapper for a List of Models
#'
#' Convenient wrapper for iterating over a list of models. The first parameter
#' of `fun` should take a registered model to be fit to the data.
#' @param model_list list of models to be fit
#' @param fun a `smpspl` function to be applied
#' @param ... parameters of `fun`
#' @export
smpspl_list <-
    function(model_list, fun, ...) {
        ret <-
            model_list |>
            purrr::map(~ fun(.x, ...)) |>
            purrr::list_rbind(names_to = "model_id")
        class(ret) <- c("smpspl_table", class(ret))
        ret
    }