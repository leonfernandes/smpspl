#' Fit multiple sample splits
#'
#' Fits model according to sample splitting on a grid of values determined by
#' `num_analysis` and `num_assessment`, where each tuple determines the number
#' of analysis and assessment sets.
#'
#'
#' @inheritParams tune::fit_resamples
#' @param data A data frame of predictors and outcomes to use when fitting the
#'  workflow.
#' @param num_analysis A positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment A positive integer specifying number of observations
#'      for goodness-of-fit tests.
#' @param formula An object of class `formula`` (or one that can be coerced to
#'      that class): a symbolic description of the model to be fitted.
#' @export
fit_splits <- function(object, ...) {
    UseMethod("fit_splits")
}