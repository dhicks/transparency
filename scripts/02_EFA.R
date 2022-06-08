library(car)
library(tidyverse)
theme_set(theme_minimal())
library(psych)
library(pander)
library(gvlma)
library(tables)
library(gghighlight)
library(lavaan)

library(visdat)
library(grid)
library(plotly)

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
d_vis |> 
# d_vis_efa |> 
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

sum_if = function(vec, condition) {
    ## Sum of values of `vec` satisfying `condition`, eg, vec > 4
    sum(vec[condition])
}

d_vis |> 
# d_vis_efa |> 
    pivot_longer(everything(), names_to = 'item', values_to = 'response') |> 
    count(item, response) |> 
    left_join(vis_labels, by = c('item' = 'tag')) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    orient(value_col = share) |> 
    group_by(item) |> 
    mutate(agree_share = sum_if(share, response >= 4)) |> 
    ungroup() |> 
    arrange(agree_share, item, response) |> 
    mutate(item = fct_inorder(item), 
           share = scales::percent(share, accuracy = 1)) |> 
    ggplot(aes(item, 
               plot_value, 
               fill = as.factor(response), 
               label = share,
               text = prompt)) +
    geom_col(data = ~ filter(.x, plot_value > 0), 
             position = position_stack(reverse = TRUE)) +
    geom_col(data = ~ filter(.x, plot_value < 0), 
             position = position_stack()) +
    xlab('ViSS item') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'share of respondents') +
    scale_fill_brewer(palette = 'RdBu', guide = 'none') +
    coord_flip() +
    theme_minimal() #+
    # theme(legend.position = 'none')

div_barplot_plotly = ggplotly(tooltip = c('x', 'label', 'text')) |> 
    hide_guides()
div_barplot_plotly

write_rds(div_barplot_plotly, here('out', '02_div_barplot.Rds'))

## Top/bottom 5 by agreement
topbottom = d_vis |> 
    pivot_longer(everything(), names_to = 'item', values_to = 'response') |> 
    count(item, response) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    summarize(agree_share = sum_if(share, response >= 4)) |> 
    ungroup() |> 
    arrange(desc(agree_share)) |> 
    slice(1:5, 32:36) |> 
    mutate(group = if_else(agree_share > .5, 'top 5', 'bottom 5')) |> 
    left_join(vis_labels, by = c('item' = 'tag')) |> 
    select(group, agree_share, item, prompt)

topbottom

write_rds(topbottom, here('out', '02_topbottom.Rds'))

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
six_clean

communalities <- 1 - apply(six_factor$loadings^2,1,sum)
communalities

## CFA ----
#lavaan package for CFA
#specify the items in each latent variable/factor

#six factor model recommended by EFA parallel analysis and inflexion point on scree plot
six_factor_model <- 'scientism =~ scientism.1 + fallible.3 + ir.2 + aims.1 + technocracy.2 + factvalue.1
                    vis =~ ir.3 + aims.2 + aims.3
                    cynicism =~ coi.1 + consensus.3 + factvalue.2 + nonsubj.1 + fallible.2 + ir.1 + coi.2 + stdpt.1
                    power =~ stdpt.3 + coi.3 + stdpt.2
                    textbook =~ consensus.2 + fallible.1 + pluralism.3 + pluralism.1 + vfi.1
                    vfi =~ vfi.3 + nonsubj.2 + technocracy.1 + factvalue.3
                    '
fit6 <- cfa(six_factor_model, data = d_vis_cfa)
summary(fit6, fit.measures = TRUE)
fitmeasures(fit6, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

#+ fig.height = 8, fig.width = 8
score_grid(fit6, d_vis_cfa)

## Correlation matrix
fa_vars = c('scientism.1',
            'fallible.3',
            'ir.2',
            'aims.1',
            'technocracy.2',
            'factvalue.1',
            'ir.3',
            'aims.2',
            'aims.3',
            'coi.1',
            'consensus.3',
            'factvalue.2',
            'nonsubj.1',
            'fallible.2',
            'ir.1',
            'coi.2',
            'stdpt.1',
            'stdpt.3',
            'coi.3',
            'stdpt.2',
            'consensus.2',
            'fallible.1',
            'pluralism.3',
            'pluralism.1',
            'vfi.2',
            'vfi.3',
            'nonsubj.2',
            'technocracy.1',
            'factvalue.3')

text_scientism = textGrob('scientism', gp=gpar(fontsize=13, fontface="bold"))
text_vis = textGrob('VIS', gp=gpar(fontsize=13, fontface="bold"))
text_cynicism = textGrob('cynicism', gp=gpar(fontsize=13, fontface="bold"))
text_power = textGrob('power', gp=gpar(fontsize=13, fontface="bold"))
text_textbook = textGrob('textbook', gp=gpar(fontsize=13, fontface="bold"))
text_VFI = textGrob('VFI', gp=gpar(fontsize=13, fontface="bold"))

six_corr_plot = d_vis_cfa |> 
    cor() |> 
    as_tibble(rownames = 'item1') |> 
    pivot_longer(-item1, names_to = 'item2', values_to = 'cor') |> 
    filter_at(vars(item1, item2), ~ .x %in% fa_vars) |> 
    mutate(across(c(item1, item2), 
                  ~ fct_relevel(.x, fa_vars))) |> 
    ggplot(aes(item1, fct_rev(item2), fill = cor)) +
    geom_raster() +
    geom_hline(yintercept = cumsum(c(4, 5, 3, 8, 3, 6))+.5) +
    geom_vline(xintercept = cumsum(rev(c(4, 5, 3, 8, 3, 6)))+.5) +
    coord_equal(clip = 'off') +
    scale_fill_gradient2(limits = c(-1, 1), name = 'R') +
    labs(x = '', 
         y = '') +
    theme(#legend.position = c(.5, 0), legend.direction = 'horizontal',
        legend.position = 'right', legend.margin = margin(),
        axis.text.x = element_text(hjust = 1L, angle = 40, vjust = 1), 
        plot.margin = margin(l = 50, r = 10, t = 5, b = 0))
six_corr_plot

ann_x = -7
six_corr_out = six_corr_plot + 
    annotation_custom(text_scientism, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 27, ymax = 27) +
    annotation_custom(text_vis, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 22, ymax = 22) +
    annotation_custom(text_cynicism, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 17, ymax = 17) +
    annotation_custom(text_power, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 11, ymax = 11) +
    annotation_custom(text_textbook, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 7, ymax = 7) +
    annotation_custom(text_VFI, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 2, ymax = 3)

six_corr_out

ggsave(here('out', '02_six_corr_matrix.png'), 
       plot = six_corr_out,
       height = 5, width = 10, scale = 1)


#three factor model based on eigenvalues > 1
three_factor_model <- ' scientism =~ scientism.1 + scientism.3 + technocracy.2 + factvalue.3 + coi.3 + aims.1 + stdpt.2 + fallible.3
                        textbook =~ pluralism.1 + nonsubj.3 + vfi.2 + pluralism.3 + technocracy.1 + fallible.1 + consensus.2 + aims.3 + aims.2 + nonsubj.2
                        cynicism =~ coi.1 + consensus.3 + nonsubj.1 + ir.1 + stdpt.1 + coi.2
                        '
fit3 <- cfa(three_factor_model, data = d_vis_cfa)
summary(fit3, fit.measures = TRUE)
fitmeasures(fit3, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

score_grid(fit3, d_vis_cfa)


## Discrete version of 6-factor model ----
six_discrete = d_vis |> 
    rownames_to_column('pid') |> 
    rowwise() |>
    mutate(fa_scientism = mean(c_across(c(scientism.1, 
                                        fallible.3,
                                        ir.2,
                                        aims.1,
                                        technocracy.2,
                                        factvalue.1))),
           fa_vis = mean(c_across(c(ir.3,
                                  aims.2,
                                  aims.3))),
           fa_cynicism = mean(c_across(c(coi.1,
                                       consensus.3,
                                       factvalue.2,
                                       nonsubj.1,
                                       fallible.2,
                                       ir.1,
                                       coi.2, 
                                       stdpt.1))),
           fa_power = mean(c_across(c(stdpt.3,
                                    coi.3,
                                    stdpt.2))),
           fa_textbook = mean(c_across(c(consensus.2,
                                       fallible.1,
                                       pluralism.3,
                                       pluralism.1,
                                       vfi.1))),
           fa_vfi = mean(c_across(c(vfi.3,
                                  nonsubj.2,
                                  technocracy.1,
                                  factvalue.3)))) |> 
    ungroup()

score_grid(scores = six_discrete)
ggsave(here('out', 'six_score_grid.png'), 
       height = 5, width = 6, scale = 1.5)

write_csv(six_discrete, here(data_dir, 'fa_six.csv'))


ggplot(six_discrete, aes(fa_scientism)) +
    geom_density() +
    geom_boxplot(width = .05, position = position_nudge(y = -.05)) +
    scale_y_continuous(guide = 'none') +
    labs(x = 'scientism', 
         y = '')

ggsave(here('out', '02_scientism.png'), 
       height = 3, width = 4, scale = 1)

ggplot(six_discrete, aes(fa_cynicism)) +
    geom_density() +
    geom_boxplot(width = .05, position = position_nudge(y = -.05)) +
    scale_y_continuous(guide = 'none') +
    labs(x = 'cynicism', 
         y = '')

ggsave(here('out', '02_cynicism.png'), 
       height = 3, width = 4, scale = 1)
