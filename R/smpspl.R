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
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl(data, 50, 100)
#' # Calculate lanyard features
#' my_acf <-
#'      function(x) {
#'          data.frame(t = x, e = 0) |>
#'              lanyard::acf_metric(t, e)
#'      }
#' o |>
#'      features(.resid, features = my_acf)
#' library(parsnip)
#' library(modeltime)
#' # Repeat with `modeltime::arima_reg`
#' arima_spec <-
#'      arima_reg() |>
#'      set_engine("arima")
#' o2 <- smpspl(arima_spec, data, 50, 100, formula = x ~ date)
#' o2 |>
#'      features(.resid, features = my_acf)
smpspl <-
    function(object, data, f_n, l_n, ...) {
        n <- vctrs::vec_size(data)
        .outcome <- autoresid::extract_outcome(object, ...)
        mdl <-
            object |>
            fit_model(.data = vctrs::vec_slice(data, 1:f_n), ...)
        smpspl_resids <-
            mdl |>
            autoresid::autoresid(new_data = data, outcome = .outcome) |>
            vctrs::vec_slice(n - l_n + 1L:l_n) |>
            tsibble::new_tsibble(class = "smpspl")
        return(smpspl_resids)
    }

#' Bootstrapped Sample Split Residuals
#'
#' @inheritParams smpspl
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @param resample_size positive integer. Number of residuals per bootstrap
#'      resample.
#' @param burn_in positive integer. Number of residuals used for burn-in.
smpspl_boot <-
    function(
        object, data, f_n, l_n, num_resamples, resample_size, burn_in = 200L,
        ...
    ) {
        if (l_n <= resample_size + burn_in) {
            rlang::warn(
                glue::glue(
                    "{resample size + burn_in} rows will be resampled from {l_n}
                    rows."
                )
            )
        } else if (l_n <= 2L * (resample_size + burn_in)) {
            rlang::inform(
                glue::glue(
                    "{resample size + burn_in} rows will be resampled from {l_n}
                    rows. Consider reducing `resample_size` or `burn_in` to
                    control overlap."
                )
            )
        }
        n <- vctrs::vec_size(data)
        .outcome <- autoresid::extract_outcome(object, ...)
        mdl <-
            object |>
            fit_model(object, .data = vctrs::vec_size(data, 1:f_n), ...)
        smpspl_resids <-
            mdl |>
            autoresid::autoresid(new_data = data, outcome = .outcome) |>
            vctrs::vec_slice(n - l_n + 1L:l_n) |>
            tsibble::new_tsibble(class = "smpspl")
        get_new_resids <-
            function(.) {
                new_resids <-
                    sample(
                        smpspl_resids$.resids, resample_size + burn_in,
                        replace = TRUE
                    )
                new_data <-
                    mdl |>
                    simts::simts(nsim = resample_size, innov = new_resids)
                smpspl(object, new_data, f_n, l_n)
            }
        purrr::map(1:num_resamples, get_new_resids)
    }