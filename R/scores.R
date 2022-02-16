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
        rename_with(~ str_c('fa_', .), .cols = where(is_lavaan_matrix)) |> 
        mutate(across(where(is_lavaan_matrix), as.numeric))
    
    return(scores_df)
}

# debugonce(scores)
# scores(fit6, d_vis)

#' Plot a scatterplot matrix of scores
#' 
#' Given a fitted model and data, calculate scores and plot scatterplots
score_grid = function(lavaan, data) {
    scores_df = scores(lavaan, data)
    
    ggplot(scores_df, aes(x = .panel_x, y = .panel_y)) +
        ggpubr::stat_cor(label.x.npc = 'center', label.y.npc = 'center', 
                         geom = 'rect', alpha = .8,
                         mapping = aes(fill = after_stat(r), 
                                       x = .panel_x, y = .panel_y,
                                       xmin = -Inf, xmax = Inf, 
                                       ymin = -Inf, ymax = Inf)) +
        geom_point(alpha = .2) + # geom_density2d() +
        ggforce::geom_autodensity(fill = 'transparent', color = 'black') +
        geom_rug(sides = 'b') +
        geom_smooth(method = 'lm') +
        ggpubr::stat_cor(label.x.npc = 'left', label.y.npc = 'center', 
                         geom = 'label', 
                         size = 5, 
                         r.accuracy = .01,
                         mapping = aes(label = after_stat(r.label), 
                                       fill = after_stat(r))) +
        ggforce::facet_matrix(vars(matches('fa_')), 
                     layer.diag = c(3, 4), 
                     layer.lower = c(1, 2, 5),
                     grid.y.diag = FALSE, 
                     switch = 'y') +
        scale_fill_gradient2(limits = c(-1, 1))
}

# score_grid(fit6, d_vis) + theme_bw()

