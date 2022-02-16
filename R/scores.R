#' Factor analysis scores
#' 
#' Given a fitted lavaan model and data
scores = function(lavaan, data) {
    is_lavaan_matrix = function(obj) {
        inherits(obj, 'lavaan.matrix')
    }

    scores_df = predict(lavaan, data) |> 
        magrittr::set_rownames(rownames(data)) |> 
        as_tibble(rownames = 'id') |> 
        mutate(across(where(is_lavaan_matrix), as.numeric))
    
    return(scores_df)
}

#' Plot a scatterplot matrix of scores
#' 
#' Given a fitted model and data, calculate scores and plot scatterplots
score_grid = function(lavaan, data) {
    scores_df = scores(lavaan, data)
    
    ggplot(scores_df, aes(x = .panel_x, y = .panel_y)) +
        geom_point(alpha = .2) + # geom_density2d() +
        ggforce::geom_autodensity(fill = 'transparent', color = 'black') +
        geom_rug(sides = 'b') +
        geom_smooth(method = 'lm') +
        ggforce::facet_matrix(vars(matches('factor')), 
                     layer.diag = c(2, 3), 
                     grid.y.diag = FALSE)
    
}

# score_grid(fit6, d_vis)

