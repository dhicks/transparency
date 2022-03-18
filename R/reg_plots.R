#' Plot the model residuals
plot_residuals = function(model) {
    model_b |> 
        broom::augment() |> 
        ggplot(aes(.fitted, .resid)) +
        geom_point() +
        stat_smooth(method = 'lm')
}


#' Plot the effects estimates
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
        geom_hline(yintercept = c(meaningful, -meaningful), linetype = 'dashed') +
        coord_flip()
}


#' Plot predictions to illustrate effects over a range of values for 1 or 2 focal variables
plot_predictions = function(model, 
                            focal_vars, 
                            step = .1, 
                            interaction_ci = FALSE,
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
            magrittr::extract2(var)
        if (!is.numeric(vec)) {
            return(unique(vec))
        }
        min = min(vec)
        max = max(vec)
        return(seq(min, max, by = step))
    }
    # debug(minmax)
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
            select(all_of(focal_vars), .fitted, .lower, .upper)
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
                                              color = focal_vars[2]))
        if (interaction_ci) {
            plot = plot + geom_ribbon(aes_string(ymin = '.lower', 
                                                 ymax = '.upper', 
                                                 fill = focal_vars[2]), 
                                      alpha = .25)
        }
        plot = plot + geom_line()
    }
    return(plot)
}

