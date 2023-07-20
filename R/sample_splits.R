#' Creates sample splits
#'
#' @param n Number of terms to sample split.
#' @param num_analysis positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment positive integer specifying number of observations for
#'      goodness-of-fit tests.
#' @export
#' @returns An object of class `smp_spl`.
sample_splits <-
    function(n, num_analysis, num_assessment) {
        # Make vectors of analysis and assessment splits
        analysis_idx <- seq(
            n / num_analysis, n,
            length.out = num_analysis
        ) |>
            as.integer()
        assessment_idx <- seq(
            n, n / num_assessment,
            length.out = num_assessment
        ) |>
            as.integer()
        settings_tbl <-
            # combine and save all sample split settings
            tidyr::expand_grid(
                analysis_idx = analysis_idx,
                assessment_idx = assessment_idx
            )
        tibble::new_tibble(settings_tbl, "smp_spl")
    }