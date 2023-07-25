#' Creates sample splits
#'
#' @param n Number of terms to sample split.
#' @inherit smpspl_grid
#' @export
#' @returns An object of class `grid_tbl`.
sample_splits <-
    function(n, num_analysis, num_assessment) {
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
            ) |>
            tidyr::nest(.assessment = assessment_idx)
        tibble::new_tibble(settings_tbl, "grid_tbl")
    }