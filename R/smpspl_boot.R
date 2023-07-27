#' Bootstrapped Sample Split Residuals
#'
#' @inheritParams smpspl
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @param resample_size positive integer. Number of residuals per bootstrap
#'      resample.
#' @param burn_in positive integer. Number of residuals used for burn-in.
#' @returns a [tsibble][tsibble::tsibble-package] of bootstrap resampled
#'      residuals.
#' @export
smpspl_boot <-
    function(
        object, data, f_n, l_n, num_resamples, resample_size, burn_in = 200L,
        ...
    ) {
        if (l_n <= resample_size + burn_in) {
            rlang::abort(
                glue::glue(
                    'Cannot sample a total of {resample_size + burn_in} rows',
                    ' from a maximum of {l_n} rows.'
                )
            )
        } else if (l_n <= 2L * (resample_size + burn_in)) {
            rlang::inform(
                glue::glue(
                    '{resample_size + burn_in} rows will be resampled from',
                    ' {l_n} rows. Consider reducing `resample_size` or ',
                    ' `burn_in` to control overlap.'
                )
            )
        }
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
        get_new_resids <-
            function(.) {
                vctrs::vec_slice(
                    smpspl_resids,
                    sample(1:n, resample_size + burn_in, replace = TRUE)
                )
            }
        ret <-
            purrr::map(1:num_resamples, get_new_resids)
        ret |>
            purrr::list_rbind(names_to = "boot_id") |>
            tsibble::as_tsibble(index = idx) |>
            tsibble::new_tsibble(class = "smpspl_boot")
    }