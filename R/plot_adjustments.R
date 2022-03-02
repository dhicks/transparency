#' Plot a DAG, showing exposure, outcome, and adjustments
plot_adjustments = function(tidydag) {
    tidydag |> 
        node_status() |> 
        dag_adjustment_sets() |> 
        ggplot(aes(x = x, y = y, 
                   xend = xend, yend = yend, 
                   color = adjusted, 
                   fill = status)) +
        geom_label(aes(label = name)) +
        geom_dag_edges() +
        theme_dag() +
        scale_fill_brewer(palette = 'Set3', 
                          na.translate = FALSE, 
                          na.value = 'white') +
        scale_color_manual(values = c('red', 'black')) +
        facet_wrap(vars(set))
}
