#' Parametric bootstrap quantiles
#'
#' Yields parametric bootstrapped metrics for specified sample splits.
#' @inheritParams fit_splits
#' @param object either of class `model_spec` or `workflow`. The fitted object
#'      should have `simts::simts` method implemented.
#' @param num_resamples positive integer. Number of bootstrap resamples.
#' @param resample_size positive integer. Number of residuals per bootstrap
#'      resample.
#' @param quantiles vector of probabalities at which quantiles of metrics are
#'      calculated.
#' @param burn_in positive integer. Number of residuals used for burn-in.
par_boot_splits <-
    function(object, ...) {
        UseMethod("par_boot_splits")
    }