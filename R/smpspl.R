#' Sample Split Residuals
#'
#' Evaluates residuals of a model for a single sample split.
#' @param object An object to fit a time series model to. Implemented models are
#'      `model_spec`, `workflow` or `mdl_defn` objects.
#' @param data A [tsibble][tsibble::tsibble-package] of predictors and outcomes
#'      to use when fitting the model.
#' @param f_n positive integer. Number of terms in analysis split.
#' @param l_n positive integer. Number of terms in assessment split.
#' @param ... Additional arguments needed to fit the data such as formula.
#' @export
#' @return a [tsibble][tsibble::tsibble-package] of class "smpspl".
#' @examples
#' library(fable)
#' library(tsibble)
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl(data, 50, 100)
smpspl <-
    function(object, data, f_n, l_n, ...) {
        n <- vctrs::vec_size(data)
        mdl <-
            object |>
            smpspltools::fit_model(.data = vctrs::vec_slice(data, 1:f_n), ...)
        smpspl_resids <-
            mdl |>
            rcits::ts2inn(ts = data) |>
            vctrs::vec_slice(n - l_n + 1L:l_n) |>
            tsibble::new_tsibble(class = "smpspl")
        return(smpspl_resids)
    }
