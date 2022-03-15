#' Plot a DAG, showing exposure, outcome, and adjustments
plot_adjustments = function(tidydag, exposure = NULL) {
    if (!is.null(exposure)) {
        tidydag = tidydag$dag |> 
            `exposures<-`(exposure) |> 
            tidy_dagitty(layout = 'kk')
    }
    
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

#' Add arrows (and thereby nodes) to a tidydag
#' 
#' @param tidydag A tidydag
#' @param arrows Character vector of arrows to add, as `foo -> bar`
add_arrows = function(tidydag, arrows) {
    tidydag$dag |> 
        str_split('\\n', simplify = TRUE) %>%
        append(., arrows, after = length(.) - 2) |> 
        str_c(collapse = '\n') |> 
        as.dagitty() |> 
        tidy_dagitty(layout = 'kk')
}