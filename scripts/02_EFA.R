library(car)
library(tidyverse)
library(psych)
library(pander)
library(gvlma)
library(tables)
library(here)
library(gghighlight)
library(lavaan)

data_dir = here("data")

source(here('R', 'vis_labels.R'))
source(here('R', 'loading_table.R'))

## Load data ----
d_clean = read_rds(here(data_dir, 'data.Rds'))

#EFA for the ViS items, checking factor structure; need to check if N is large enough for a split to do CFA on
d_vis <- subset(d_clean, select = c(ViS01:ViS36)) |> 
    na.omit() |> 
    mutate(across(everything(), as.numeric)) |> 
    set_names(vis_labels$tag)

#if N is big enough to reasonably split for EFA and CFA,
set.seed(032585) #for reproducibility
dummy_sep <- rbinom(nrow(d_vis), 1, 0.5) #create dummy indicator to randomly split sample
d_vis_efa <- d_vis[dummy_sep == 0, ] #extract data where dummy == 0
d_vis_cfa <- d_vis[dummy_sep == 1, ] #extract data where dummy == 1


## Descriptive visualizations ----
## Share of respondents (agreeing or strongly agreeing)
d_vis_efa |> 
    pivot_longer(everything(), names_to = 'item', values_to = 'value') |> 
    count(item, value) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    # filter(value >= 4) |> 
    ggplot(aes(fct_rev(item), share, fill = as.factor(value))) +
    geom_col() +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    gghighlight(value >= 4,
                unhighlighted_params = list(fill = NULL,
                                            alpha = .5)) +
    xlab('ViSS item') +
    scale_y_continuous(labels = scales::percent_format()) +
    # scale_fill_viridis_d(option = 'A', guide = 'none', direction = -1) +
    scale_fill_brewer(palette = 'RdBu', guide = 'none') +
    coord_flip() +
    theme_minimal()

## Diverging bar plot
orient = function(count_df, level_col = response, ref_level = 3, value_col = n) {
    ## Set the orientation of each level's bar as `plot_value`
    ## The reference level gets two bars, one positive and one negative
    ## Based on <https://stackoverflow.com/questions/51201852/faceted-horizontal-divergent-stacked-bar-plot-including-negative-values-using-dp/51217969#51217969>
    ref_negative = count_df |> 
        filter({{level_col}} == ref_level) |> 
        mutate(plot_value = -{{value_col}}/2)
    
    count_df |> 
        mutate({{value_col}} := as.numeric({{value_col}}),
               plot_value     = case_when({{level_col}}  <  ref_level ~ -{{value_col}}, 
                                          {{level_col}} == ref_level  ~ {{value_col}}/2, 
                                          {{level_col}}  >  ref_level ~ {{value_col}})) |> 
        bind_rows(ref_negative)
}


d_vis_efa |> 
    pivot_longer(everything(), names_to = 'item', values_to = 'response') |> 
    count(item, response) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    orient(value_col = share) |> 
    arrange(item, response) |> 
    ggplot(aes(fct_rev(item), plot_value, fill = as.factor(response))) +
    geom_col(data = ~ filter(.x, plot_value > 0), 
             position = position_stack(reverse = TRUE)) +
    geom_col(data = ~ filter(.x, plot_value < 0), 
             position = position_stack()) +
    xlab('ViSS item') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'share of respondents') +
    scale_fill_brewer(palette = 'RdBu', guide = 'none') +
    coord_flip() +
    theme_minimal()

## checking EFA assumptions ----
cor.matrix <- cor(d_vis_efa)
bartlett.test(d_vis_efa) #testing whether correlation matrix is significantly different from identity matrix
KMO(d_vis_efa) #checking adequacy of sample size
det(cor.matrix) #checking for possible multicollinearity

## EFA ----
vis_fa <- fa.parallel(d_vis_efa, fm = "minres", fa = "fa")

#check item loadings for N factor solutions (look for substantial cross-loadings or no substantial loadings)
#do for as many factor solution options as seems prudent; what is recommended by fa.parallel, visual inspection of scree, or eigenvalues
three_factor <- fa(d_vis_efa, nfactors = '3', rotate ="varimax")
six_factor <- fa(d_vis_efa, nfactors = '6', rotate ="varimax")
#writing factor loadings to a csv for easier inspection
loading_table(three_factor, path = here(data_dir, "three_factor_loadings.csv"))
loading_table(six_factor,   path = here(data_dir, "six_factor_loadings.csv"))

## CFA ----
#lavaan package for CFA
#specify the items in each latent variable/factor

#six factor model recommended by EFA parallel analysis and inflexion point on scree plot
six_factor_model <- ' factor_1 =~ item1 + item2
                    factor_2 =~ item3 + item4
                    factor_3 =~ item5 + item6
                    factor_4 =~ item7 + item8
                    factor_5 =~ item9 + item10
                    factor_6 =~ item11 + item12
                    '
fit6 <- cfa(six_factor_model, data = d_vis_cfa)
summary(fit6, fit.measures = TRUE)
fitmeasures(fit6, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

#three factor model based on eigenvalues > 1
three_factor_model <- ' factor_1 =~ item1 + item2
                        factor_2 =~ item3 + item4
                        factor_3 =~ item 5 + item6
                        '
fit3 <- cfa(three_factor_model, data = d_vis_cfa)
summary(fit3, fit.measures = TRUE)
fitmeasures(fit3, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))
