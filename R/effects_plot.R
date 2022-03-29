# model = lm(meti_mean ~ fa_scientism + fa_vis + fa_cynicism*political_ideology +
#                fa_power + fa_textbook + fa_vfi +
#                age + gender + #race_ethnicity + religious_affil + 
#                religious_serv +
#                political_ideology + political_affiliation + education, 
#            data = filter(dataf, disclosure))

plot_effects = function(model, 
                        focal_vars, 
                        step = .1, 
                        return_plot = TRUE) {
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    ## Values to use for the non-focal variables
    defaults = model$model |> 
        select(-meti_mean) |> 
        summarize(across(c(where(is.numeric), where(is.logical)),
                         mean, na.rm = TRUE), 
                  across(c(where(is.factor)), 
                         getmode)) |> 
        as.list() |> 
        magrittr::inset(focal_vars, NULL)
    
    ## For focal variables, ranges
    minmax = function(var, step = .1) {
        vec = model |> 
            magrittr::extract2('model') |> 
            magrittr::extract(var)
        min = min(vec)
        max = max(vec)
        return(seq(min, max, by = step))
    }
    # minmax(focal_vars[1])
    
    ## Build new data for predict
    newdata = focal_vars |> 
        set_names() |> 
        map(minmax, step) |> 
        c(defaults) |>
        cross_df()
    
    predictions = broom::augment(model, 
                                 newdata = newdata, 
                                 interval = 'confidence')
    
    if (!return_plot) {
        ## If we're not returning a plot, return the predictions df
        ## with just the focal variables and .fitted
        pred_df = predictions |> 
            select(all_of(focal_vars), .fitted)
        return(pred_df)
    }
    
    if (identical(length(focal_vars), 1L)) {
        ## Single-variable plot
        plot = ggplot(predictions, aes_string(focal_vars, '.fitted')) +
            geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .25) +
            geom_line()
    } else if (identical(length(focal_vars), 2L)) {
        ## Interaction plot
        plot = ggplot(predictions, aes_string(focal_vars[1], '.fitted', 
                              group = focal_vars[2], 
                              color = focal_vars[2])) +
            geom_line()
    }
    return(plot)
}

# effects_plot(model, 'fa_cynicism', return_plot = FALSE)
# effects_plot(model, c('political_ideology', 'fa_cynicism'), 
#              return_plot = FALSE)
# 
# effects_plot(model, 'fa_cynicism') +
#     coord_cartesian(ylim = c(3, 7))
# effects_plot(model, c('political_ideology', 'fa_cynicism'), step = .1) +
#     ylim(3, 7)
