reg_tbl = function(models, 
                   labs = c('**EMAD**', '**HL replication**'), 
                   filters = !variable %in% c('gender', 
                                              'race_ethnicity', 
                                              'religious_affil')) {
    # filters = enquos(filters)
    models |> 
        map(tbl_regression, intercept = TRUE) |> 
        map(add_glance_table, include = c(r.squared, nobs, 
                                          adj.r.squared,
                                          statistic, p.value)) |> 
        tbl_merge(tab_spanner = labs) |> 
        modify_table_body(~ arrange(.x, row_type == "glance_statistic")) |> 
        modify_table_body(~ filter(.x, {{filters}}))
}

# list(emad = model_b_emad, 
#      hl = model_b) |> 
#     reg_tbl()

write_reg_tbl = function(tbl, path) {
    tbl |> 
        as_flex_table() |> 
        flextable::save_as_docx(path = str_c(path, '.docx'),
                                pr_section = officer::prop_section(
                                    page_size = officer::page_size(width = 11, 
                                                                   orient = 'landscape')
                                )
        )
    ## Problems exporting gt to PDF are a known issue
    ## <https://github.com/rstudio/gt/issues/721>
    tbl |> 
        as_gt() |> 
        gtsave(filename = str_c(path, '.pdf'), zoom = 1)
    
    tbl |> 
        tbl_butcher() |> 
        # as_gt() |> 
        write_rds(file = str_c(path, '.Rds'))
}
