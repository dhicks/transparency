loading_table = function(model, 
                         threshold = .3, 
                         quiet = FALSE, 
                         path = NULL, 
                         overwrite = FALSE) {
    loadings_unfltd = model |> 
        ## Extract loadings and tidy
        loadings() |> 
        unclass() |> 
        as_tibble(rownames = 'item') |> 
        ## Filter based on threshold
        pivot_longer(-item, names_to = 'factor', values_to = 'loading')
    
    ## Dropped items
    if (!quiet) {
        loadings_df = filter(loadings_unfltd, abs(loading) > threshold)
        dropped_df = loadings_unfltd |> 
            pivot_wider(names_from = 'factor', values_from = 'loading') |> 
            anti_join(pivot_wider(loadings_df, names_from = 'factor', values_from = 'loading'), 
                      by = 'item')
        message('Dropped items')
        print(dropped_df)
    }
    
    ## Cross-loaded items
    if (!quiet) {
        xload_df = loadings_df |>
            add_count(item) |>
            filter(n > 1) |>
            select(-n) |>
            pivot_wider(names_from = 'factor', values_from = 'loading')
        message('Cross-loaded items')
        print(xload_df)
    }
    
    loadings_clean = loadings_df |>
        anti_join(xload_df, by = 'item') |>
        pivot_wider(names_from = 'factor', values_from = 'loading') |>
        arrange(across(starts_with('MR'), desc)) |> 
        left_join(vis_labels, by = c('item' = 'tag')) |> 
        select(-item.y)
    
    if (!is.null(path)) {
        if (!file.exists(path) || overwrite) {
            write_csv(loadings_clean, path, na = '')
        }
        if (file.exists(path) && !overwrite) {
            warning('CSV already exists; skipping overwrite')
        }
    }
    
    return(loadings_clean)
}

