#' Bootstrapped Sample Split Residuals
#'
#' @inheritParams smpspl
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @returns a [tsibble][tsibble::tsibble-package] of bootstrap resampled
#'      residuals.
#' @export
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
            vctrs::vec_slice(n - l_n + 1:l_n) |>
            tsibble::new_tsibble(class = "smpspl")
        idx <- tsibble::index(smpspl_resids)
        p <- progressr::progressor(num_resamples)
        get_new_resids <-
            function(.) {
                p()
                vctrs::vec_slice(
                    smpspl_resids,
                    sample(1:l_n, l_n, replace = TRUE)
                )
            }
        ret <-
            furrr::future_map(1:num_resamples, get_new_resids)
        ret |>
            purrr::list_rbind(names_to = "boot_id") |>
            tsibble::as_tsibble(index = idx, key = "boot_id") |>
            tsibble::new_tsibble(class = "smpspl_boot")
    }