plot_residuals = function(model) {
    model_b |> 
        broom::augment() |> 
        ggplot(aes(.fitted, .resid)) +
        geom_point() +
        stat_smooth(method = 'lm')
}

plot_estimate = function(models, ...) {
    extract_estimates = function(model, ...) {
        model |> 
            broom::tidy(conf.int = TRUE) |> 
            filter(...)
    }
    
    estimates_df = map_dfr(models, extract_estimates, ..., .id = 'study')
    # return(estimates_df)
    
    ggplot(estimates_df, 
           aes(term, estimate, 
               ymin = conf.low, ymax = conf.high, 
               color = study)) +
        geom_pointrange(position = position_dodge(width = .25)) +
        geom_hline(yintercept = c(threshold, -threshold), linetype = 'dashed') +
        coord_flip()
}

    