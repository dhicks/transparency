library(car)
library(tidyverse)
theme_set(theme_bw())
library(psych)
library(pander)
library(gvlma)
library(tables)
library(gghighlight)
library(lavaan)

library(visdat)

library(here)

data_dir = here("data")

source(here('R', 'vis_labels.R'))
source(here('R', 'loading_table.R'))
source(here('R', 'scores.R'))

## Should the script overwrite existing loadings tables? 
overwrite_loading_tables = FALSE

## Load data ----
d_clean = read_rds(here(data_dir, 'data.Rds')) |> 
    select(pid, ViS01:ViS36) |> 
    mutate(across(-pid, as.numeric)) |> 
    set_names('pid', vis_labels$tag)

## 0.3% missing values
vis_miss(d_clean)
## Items with missing values don't appear to cluster
vis_miss(d_clean, cluster = TRUE)

## 906 complete cases; 
## 69 missing 1, 11 missing 2, 2 missing 3
d_clean |> 
    rowwise() |> 
    summarize(missing = sum(is.na(c_across(-pid)))) |> 
    count(missing)


#EFA for the ViS items, checking factor structure; need to check if N is large enough for a split to do CFA on
d_vis <- d_clean |> 
    column_to_rownames('pid') |> 
    na.omit()

#if N is big enough to reasonably split for EFA and CFA,
set.seed(032585) #for reproducibility
dummy_sep <- rbinom(nrow(d_vis), 1, 0.5) #create dummy indicator to randomly split sample
d_vis_efa <- d_vis[dummy_sep == 0, ] #extract data where dummy == 0
d_vis_cfa <- d_vis[dummy_sep == 1, ] #extract data where dummy == 1

vis_cor(d_vis_efa)

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
bartlett <- bartlett.test(d_vis_efa) #testing whether correlation matrix is significantly different from identity matrix
kmo <- KMO(d_vis_efa) #checking adequacy of sample size
det <- det(cor.matrix) #checking for possible multicollinearity

## EFA ----
vis_fa <- fa.parallel(d_vis_efa, fm = "minres", fa = "fa")

#check item loadings for N factor solutions (look for substantial cross-loadings or no substantial loadings)
#do for as many factor solution options as seems prudent; what is recommended by fa.parallel, visual inspection of scree, or eigenvalues
three_factor <- fa(d_vis_efa, nfactors = '3', rotate ="varimax")
six_factor <- fa(d_vis_efa, nfactors = '6', rotate ="varimax")

#writing factor loadings to a csv for easier inspection
three_clean = loading_table(three_factor, 
                            path = here(data_dir, 
                                        "three_factor_loadings.csv"), 
                            overwrite = overwrite_loading_tables)
six_clean = loading_table(six_factor, 
                          path = here(data_dir, 
                                      "six_factor_loadings.csv"), 
                          overwrite = overwrite_loading_tables)

communalities <- 1 - apply(six_factor$loadings^2,1,sum)

## CFA ----
#lavaan package for CFA
#specify the items in each latent variable/factor

#six factor model recommended by EFA parallel analysis and inflexion point on scree plot
six_factor_model <- 'scientism =~ scientism.1 + fallible.3 + ir.2 + aims.1 + technocracy.2 + factvalue.1
                    vis =~ ir.3 + aims.2 + aims.3
                    cynicism =~ coi.1 + consensus.3 + factvalue.2 + nonsubj.1 + fallible.2 + ir.1 + coi.2
                    stdpt =~ stdpt.3 + coi.3 + stdpt.2
                    textbook =~ consensus.2 + fallible.2 + pluralism.3 + pluralism.1 + vfi.1
                    vfi =~ vfi.3 + nonsubj.2 + technocracy.1 + factvalue.3
                    '
fit6 <- cfa(six_factor_model, data = d_vis_cfa)
summary(fit6, fit.measures = TRUE)
fitmeasures(fit6, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

score_grid(fit6, d_vis_cfa)

#three factor model based on eigenvalues > 1
three_factor_model <- ' scientism =~ scientism.1 + scientism.3 + technocracy.2 + factvalue.3 + coi.3 + aims.1 + stdpt.2 + fallible.3
                        textbook =~ pluralism.1 + nonsubj.3 + vfi.2 + pluralism.3 + technocracy.1 + fallible.1 + consensus.2 + aims.3 + aims.2 + nonsubj.2
                        cynicism =~ coi.1 + consensus.3 + nonsubj.1 + ir.1 + stdpt.1 + coi.2
                        '
fit3 <- cfa(three_factor_model, data = d_vis_cfa)
summary(fit3, fit.measures = TRUE)
fitmeasures(fit3, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

score_grid(fit3, d_vis_cfa)
