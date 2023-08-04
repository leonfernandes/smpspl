#' Bootstrapped Sample Split Residuals
#'
#' @inheritParams smpspl
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @importFrom rlang `:=`
#' @returns a [tsibble][tsibble::tsibble-package] of bootstrap resampled
#'      residuals.
#' @export
#' @examples
#' \dontrun{
#' library(fable)
#' data <-
#'      tsibble::tsibble(x = rnorm(100), date = Sys.Date() + 0:99, index = date)
#' # Consider an AR(1) model
#' o <-
#'      ARIMA(x ~ pdq(1, 0, 0) + PDQ(0, 0, 0)) |>
#'      smpspl_boot(data, 50, 100, 100)
#'
#' # Calculate lanyard features
#' my_acf <-
#'      function(x) {
#'          data.frame(t = x, e = 0) |>
#'              lanyard::acf_metric(t, e) |>
#'              tidy()
#'      }
#' library(smpspltools)
#' o |>
#'      features(.resid, features = my_acf)
#' }
smpspl_boot <-
    function(
        object, data, f_n, l_n, num_resamples, ...
    ) {
        .resid <- rlang::sym(".resid")
        n <- vctrs::vec_size(data)
        .outcome <- smpspltools::extract_outcome(object, ...)
        mdl <-
            object |>
            smpspltools::fit_model(.data = vctrs::vec_slice(data, 1:f_n), ...)
        smpspl_resids <-
            mdl |>
            autoresid::autoresid(new_data = data, outcome = .outcome) |>
            tsibble::as_tibble() |>
            dplyr::select(!!.resid) |>
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
        ret <-
            furrr::future_map(1:num_resamples, get_new_resids)
        ret |>
            purrr::list_rbind(names_to = "boot_id") |>
            tsibble::as_tsibble(index = idx, key = "boot_id") |>
            tsibble::new_tsibble(class = "smpspl_boot")
    }