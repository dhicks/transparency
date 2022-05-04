dataf |> 
    filter(!is.na(part_values)) |> 
    ggplot(aes(conclusion, meti_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))

summary(model_d)
summary(model_ed)

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(fa_cynicism, meti_mean, color = shared_values)) +
    geom_point() +
    stat_smooth(method = lm)

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(fa_power, meti_mean, color = shared_values)) +
    geom_point() +
    stat_smooth(method = lm)

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(fa_scientism, meti_mean, color = sci_values)) +
    geom_point() +
    stat_smooth(method = lm)

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(fa_power, meti_mean, color = sci_values)) +
    geom_point() +
    stat_smooth(method = lm)



lm(meti_mean ~ part_values + fa_scientism, 
   data = dataf) |> 
    summary()

lm(meti_mean ~ part_values * fa_scientism, 
   data = dataf) |> 
    summary()


cross_df(list(sci_values = c('public health', 'economic growth'), 
              fa_scientism = seq(1, 5, by = .1), 
              fa_vis = 3, 
              fa_cynicism = 3,
              fa_power = 3, 
              fa_textbook = 3, 
              fa_vfi = 3)) %>%
    broom::augment(model_fd, newdata = ., interval = 'confidence') |> 
    ggplot(aes(fa_scientism, .fitted, group = sci_values, 
               color = sci_values, fill = sci_values)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .25) +
    geom_line()

cross_df(list(sci_values = c('public health', 'economic growth'), 
              fa_scientism = 3, 
              fa_vis = 3, 
              fa_cynicism = 3,
              fa_power = seq(1, 5, by = .1), 
              fa_textbook = 3, 
              fa_vfi = 3)) %>%
    broom::augment(model_fd, newdata = ., interval = 'confidence') |> 
    ggplot(aes(fa_power, .fitted, group = sci_values, 
               color = sci_values, fill = sci_values)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .25) +
    geom_line()

