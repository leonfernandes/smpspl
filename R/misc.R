quantile_df <-
    function(x, probs = c(.05, .25, .5, .75, .95)) {
        tibble::tibble(
            val = stats::quantile(x, probs, na.rm = TRUE),
            quant = probs
        )
    }