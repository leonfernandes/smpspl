quantile_df <-
    function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
        tibble::tibble(
            val = stats::quantile(x, probs, na.rm = TRUE),
            quant = probs
        )
    }