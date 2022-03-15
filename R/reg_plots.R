plot_residuals = function(model) {
    model_b |> 
        broom::augment() |> 
        ggplot(aes(.fitted, .resid)) +
        geom_point() +
        stat_smooth(method = 'lm')
}

plot_estimate = function(model, exposure) {
    model |> 
        broom::tidy(conf.int = TRUE) |> 
        filter(str_detect(term, exposure)) |> 
        ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
        geom_pointrange() +
        geom_hline(yintercept = c(threshold, -threshold), linetype = 'dashed') +
        coord_flip()
}

    