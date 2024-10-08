#' Bootstrapped Sample Split Residuals
#'
#' @inheritParams smpspl
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @inheritParams furrr::future_map
#' @importFrom rlang `:=`
#' @returns a [tsibble][tsibble::tsibble-package] of bootstrap resampled
#'      residuals.
#' @export
#' @examples
#' library(fable)
#' library(tsibble)
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl_boot(data, 50, 100, 20)
smpspl_boot <-
    function(
        object, data, f_n, l_n, num_resamples,
        .options = furrr::furrr_options(), ...
    ) {
        inn_sym <- rlang::sym("inn")
        n <- vctrs::vec_size(data)
        mdl <-
            object |>
            smpspltools::fit_model(.data = vctrs::vec_slice(data, 1:f_n), ...)
        smpspl_resids <-
            mdl |>
            rcits::ts2inn(ts = data) |>
            tsibble::as_tibble() |>
            dplyr::select(!!inn_sym) |>
            vctrs::vec_slice(n - l_n + 1:l_n)
        idx <- tsibble::index(data)
        p <- progressr::progressor(num_resamples)
        get_new_resids <-
            function(.) {
                p()
                smpspl_resids |>
                    vctrs::vec_slice(
                        sample(1:l_n, l_n, replace = TRUE)
                    ) |>
                    dplyr::mutate({{idx}} := Sys.Date() + 1:l_n - 1) |>
                    tsibble::as_tsibble(index = idx)
            }
        my_crt <-
            carrier::crate(~ get_new_resids(.x))
        ret <-
            furrr::future_map(1:num_resamples, my_crt, .options = .options)
        ret |>
            purrr::list_rbind(names_to = "boot_id") |>
            tsibble::as_tsibble(index = idx, key = "boot_id") |>
            tsibble::new_tsibble(class = "smpspl_boot")
    }