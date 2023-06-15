#' Creates sample splits
#'
#' @param data A data frame of predictors and outcomes.
#' @param num_analysis positive integer specifying number of observations for
#'      analysis.
#' @param num_assessment positive integer specifying number of observations for
#'      goodness-of-fit tests.
#' @export
#' @returns An object of class `smp_spl`.
sample_splits <-
    function(data, num_analysis, num_assessment) {
        n <- vctrs::vec_size(data)
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
            ) |>
            # remove settings which don't use all the data
            dplyr::filter(analysis_idx + assessment_idx >= n) |>
            # make id column
            dplyr::mutate(
                split_id = paste0(
                    "split", stringr::str_pad(dplyr::row_number(), 2, pad = 0)
                )
            )
        tibble::new_tibble(settings_tbl, "smp_spl")
    }