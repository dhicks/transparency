#' ---
#' title: "DAG analysis"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

library(tidyverse)
theme_set(theme_minimal())
library(broom)
library(patchwork)
library(ggbeeswarm)
library(dagitty)
library(ggdag)
## <https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html>
library(gt)
library(flextable)
library(officer)
library(gtsummary)

## Need Hmisc for bootstrap CIs in some plots, but don't want to load it
find.package('Hmisc')

library(here)
## Helper functions for regression tables, DAG visualization, and effects and prediction plots for regression models
source(here('R', 'reg_tbl.R'))
source(here('R', 'plot_adjustments.R'))
source(here('R', 'reg_plots.R'))

## More readable contrasts in regression models
library(car)
options(contrasts = c('contr.Treatment', 'contr.poly'))
options(decorate.contr.Treatment = '')

out_dir = here('out')
if (!dir.exists(out_dir)) {
    dir.create(out_dir)
}
data_dir = here('data')

#' # Load data
## Load data ----
## Elliott et al. data
emad_df = read_rds(here(data_dir, 'emad.Rds'))

## Our data
dataf = read_rds(here(data_dir, 'data.Rds'))

#' # Descriptive summary of our data
## Descriptive summary of our data ----
#' - age
#' - gender
#' - race/ethnicity
#'    - <https://www.census.gov/library/visualizations/interactive/race-and-ethnicity-in-the-united-state-2010-and-2020-census.html>
#'    - overrep White (72% vs. 62%)
#'    - underrep Hispanic (3% vs. 19%)
#'    - Black and A/PI represented accurately at 13% and 6%
#' - religious affiliation
#' - religious services
#' - political ideology
#' - political affiliation
#' - education
#'    - <https://www.statista.com/statistics/184260/educational-attainment-in-the-us/>
#'    - underrep non-HS grads (1% vs. 9%)
#'    - overrep college grads (57% vs. 38%)
#' - participant values
#' 

#' Several of our variables allow respondents to select multiple values, eg, race-ethnicity. These are downloaded as factors, with levels such as `5` or `2,4,5`.  We recode these as characters for the demographic summary table.  Because these are just used as adjustment variables in the analysis, we don't recode them for use below. 

re_labels = c('American Indian or Alaskan Native', 
              'Asian or Pacific Islander', 
              'Black', 
              'Hispanic', 
              'White', 
              'Other', 
              'Prefer not to answer')
relig_labels = c('Buddhist', 
                 'Catholic', 
                 'Hindu', 
                 'Jewish', 
                 'Muslim', 
                 'Protestant', 
                 'No religion', 
                 'Other', 'Prefer not to answer')
relig_serv_labels = c('Never', 
                      'A few times per year', 
                      'Once every month or two', 
                      '2-3 times per month', 
                      'Once per week', 
                      'More than once per week', 
                      'Daily')
poli_id_labels = c('Strongly liberal', 
                   'Moderately liberal', 
                   'Mildly liberal', 
                   'Centrist', 
                   'Mildly conservative', 
                   'Moderately conservative', 
                   'Strongly conservative', 
                   'Other', 
                   'Prefer not to answer')
poli_aff_labels = c('Democratic party', 
                    'Republican party', 
                    'Independent/no party', 
                    'Other', 
                    'Prefer not to answer')
edu_labels = c('Less than high school', 
               'High school, or some college', 
               'Bachelor’s degree or higher')
fix_multifac = function(vec, labs, ordered = FALSE) {
    chr = vec |> 
        as.character() |> 
        str_split(',') |> 
        map(~ labs[as.integer(.x)]) |> 
        map_chr(str_c, collapse = '/')
    if (!ordered) {
        return(chr)
    } else {
        fct_relevel(chr, labs)
    }
}

demo_gt = dataf |> 
    select(pid, age, gender, race_ethnicity, 
           religious_affil, religious_serv,
           political_ideology, political_affiliation, 
           education, part_values, disclosure) |> 
    mutate(gender = fct_drop(gender),
           race_ethnicity = fix_multifac(race_ethnicity, re_labels), 
           religious_affil = fix_multifac(religious_affil, relig_labels), 
           religious_serv = fix_multifac(religious_serv, 
                                         relig_serv_labels, 
                                         ordered = TRUE), 
           political_ideology = fix_multifac(political_ideology, 
                                             poli_id_labels, 
                                             ordered = TRUE),
           political_affiliation = fix_multifac(political_affiliation, 
                                                poli_aff_labels), 
           education = fix_multifac(education, edu_labels, ordered = TRUE)) |>
    select(-pid) |> 
    tbl_summary(label = list(race_ethnicity ~ 'race/ethnicity', 
                             religious_affil ~ 'religious affiliation', 
                             religious_serv ~ 'religious service attendance', 
                             political_ideology ~ 'political ideology', 
                             political_affiliation ~ 'political affiliation', 
                             part_values ~ 'participant values'), 
                sort = list(race_ethnicity ~ 'frequency', 
                            religious_affil ~ 'frequency')) |> 
    bold_labels()

demo_gt |> 
    as_flex_table() |> 
    save_as_docx(path = here(out_dir, '03_demo_table.docx'), 
                 pr_section = prop_section(
                     page_size = page_size(orient = "landscape")
                 )
    )


#' # Trust, overall
## Trust, overall ----
ggplot() +
    geom_violin(aes(x = 'EMAD', pa_mean), 
                draw_quantiles = .5,
                data = emad_df) +
    geom_beeswarm(aes(x = 'EMAD', pa_mean), 
                  data = emad_df) +
    geom_violin(aes(x = 'HL', meti_mean), 
                draw_quantiles = .5, 
                data = dataf) +
    geom_beeswarm(aes(x = 'HL', meti_mean), 
                  data = dataf) +
    ylab('mean trust')

#' Across our dataset, standard deviation of mean trust is 1.3 on the 1-7 scale
sd(dataf$meti_mean)

#' # H1. Modest correlation between values and ideology #
## H1. Modest correlation between values and ideology ----
#' \(i) Political liberals are more likely to prioritize public health over economic growth, compared to political conservatives; but (ii) a majority of political conservatives prioritize public health.
#' 
#' NB 1. No DAG here because this isn't a causal claim.  2. Direction of ideology coding is reversed between the two studies. 
#' 
#' Compared to Elliott et al., our strong conservatives placed lower value on public health, and overall conservatives are about 50-50. 

emad_df |>
    count(ideology, tradeoff) |>
    group_by(ideology) |>
    mutate(share = n / sum(n)) |>
    ungroup() |>
    ggplot(aes(ideology, n, fill = as.factor(tradeoff))) +
    geom_col() +
    scale_fill_viridis_d()

last_plot() + aes(y = share)

part_values_plot = dataf |> 
    filter(!is.na(pref)) |> 
    count(political_ideology, part_values) |> 
    group_by(political_ideology) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    ggplot(aes(political_ideology, n, fill = part_values)) +
    geom_col(color = 'black') +
    scale_x_continuous(labels = NULL, 
                       name = '← liberal                   conservative →\npolitical ideology') +
    scale_fill_viridis_d(option = 'E', name = 'participant\nvalues')
part_values_plot

part_values_share = part_values_plot + aes(y = share) +
    scale_y_continuous(labels = scales::percent_format())
part_values_share

part_values_plot + part_values_share +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A')
ggsave(here(out_dir, '03_part_values.png'), 
       height = 4, width = 8, dpi = 200, scale = 1.5)

table(dataf$political_ideology, dataf$pref)

dataf |> 
    mutate(political_ideology = case_when(
        political_ideology < 4 ~ 'liberal', 
        political_ideology == 4 ~ 'moderate', 
        political_ideology > 4 ~ 'conservative'
    )) |> 
    count(political_ideology)

cor(emad_df$ideology, emad_df$tradeoff, 
    use = 'complete.obs',
    method = 'spearman')

cor(as.integer(dataf$political_ideology), as.integer(dataf$pref), 
    use = 'complete.obs', 
    method = 'spearman')

glm(I(part_values == 'economic growth') ~ political_ideology, 
    family = 'binomial', 
    data = dataf) |> 
    summary()

#' # DAG #
## DAG ----
#' We use the following DAG throughout the rest of this analysis
dag = dagify(METI ~ shared_values + sci_values +
                 part_values + demographics,
             shared_values ~ part_values + sci_values, 
             part_values ~ demographics,
             outcome = 'METI') |> 
    tidy_dagitty(layout = 'kk')

ggplot(dag, aes(x = x, y = y, 
                xend = xend, yend = yend)) +
    geom_label(aes(label = name)) +
    geom_dag_edges() +
    coord_cartesian(clip = 'off') +
    theme_dag()


#' # H2. Consumer risk sensitivity #
## H2. Consumer risk sensitivity ----
#' *Scientists who find that a chemical harms human health are perceived as more trustworthy than scientists who find that a chemical does not cause harm.*

ggplot(emad_df, aes(conclusion, pa_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

ggplot(dataf, aes(conclusion, meti_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

#' Because the conclusion is experimentally manipulated, we don't need any adjustments.  

dag |> 
    add_arrows('conclusion -> METI') |> 
    plot_adjustments(exposure = 'conclusion') +
    scale_color_manual(values = 'black')

model_b_emad = lm(pa_mean ~ conclusion, data = emad_df)
summary(model_b_emad)
model_b = lm(meti_mean ~ conclusion, data = dataf)
summary(model_b)
plot_residuals(model_b)

plot_estimate(list(emad = model_b_emad, 
                   hl = model_b),
              str_detect(term, 'conclusion'))

tbl_regression(model_b, intercept = TRUE) |> 
    add_glance_table(include = c(r.squared, nobs, statistic, p.value))

list(emad = model_b_emad, 
     hl = model_b) |> 
    reg_tbl()

#' # H3. Transparency penalty #
## H3. Transparency penalty ----
#' *Scientists who disclose values are perceived as less trustworthy than scientists who do not.* 

trans_plot_emad = ggplot(emad_df, aes(disclosure, pa_mean)) +
    # geom_violin(draw_quantiles = .5) +
    geom_beeswarm(alpha = .25, size = .3) +
    stat_summary(fun.data = mean_cl_boot, color = 'red', 
                 size = 1, fatten = 0) +
    stat_summary(geom = 'line', group = 1L, color = 'red') +
    labs(y = 'trust')
trans_plot_emad


trans_plot_us = ggplot(dataf, aes(disclosure, meti_mean)) +
    geom_beeswarm(alpha = .25, size = .3) +
    stat_summary(fun.data = mean_cl_boot, color = 'red', 
                 size = 1, fatten = 0) +
    stat_summary(geom = 'line', group = 1L, color = 'red') +
    labs(y = 'trust')
trans_plot_us

trans_plot_emad + 
    ggtitle('EMAD') +
    trans_plot_us +
    ggtitle('HL replication')

ggsave(here(out_dir, '03_transparency.png'), 
       height = 3, width = 6, scale = 1, 
       bg = 'white')

#' Again, disclosure/transparency is experimentally controlled, so no adjustment is required. 
dag |> 
    add_arrows('disclose -> METI') |> 
    plot_adjustments('disclose') + 
    scale_color_manual(values = 'black')

model_c_emad = lm(pa_mean ~ disclosure, data = emad_df)
summary(model_c_emad)
model_c = lm(meti_mean ~ disclosure, data = dataf)
summary(model_c)
plot_residuals(model_c)
# plot_estimate(model_c, 'disclosure')
plot_estimate(list(emad = model_c_emad, 
                   hl = model_c), 
              str_detect(term, 'disclosure'))

list(emad = model_c_emad, 
     hl = model_c) |> 
    reg_tbl()

#' ## H2 + H3 combined table
## H2 + H3 combined table ----
model_bc_emad = lm(pa_mean ~ conclusion + disclosure, data = emad_df)
model_bc = lm(meti_mean ~ conclusion + disclosure, data = dataf)

bc_tbl = list(emad = model_bc_emad, 
              hl = model_bc) |> 
    reg_tbl()
bc_tbl
write_reg_tbl(bc_tbl, here(out_dir, '03_bc_tbl'))


#' # H4. Shared values #
## H4. Shared values ----
#' *Given that the scientist discloses values, if the participant and the scientist share the same values, the scientist is perceived as more trustworthy than if the participant and scientist have discordant values.* 
#' 
#' Quick descriptive plots suggest support for this hypothesis.  But we need to consider possible confounding. 

emad_df |> 
    filter(disclosure, !is.na(part_values)) |> 
    ggplot(aes(shared_values, pa_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

dataf |> 
    filter(disclosure, !is.na(part_values)) |> 
    ggplot(aes(shared_values, meti_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

#' ## H4 DAG discussion
## H4 DAG discussion ----
#' In our actual situation, we only need to adjust for `participant values` and `scientist values`; `participant values` is on every back-door path running through demographics.  

plot_adjustments(dag, 'shared_values') +
    coord_cartesian(clip = 'off')

ggsave(here(out_dir, '03_shared_values_dag.png'), 
       height = 3, width = 6, dpi = 200, scale = 1.5)

#' However, if `participant values` and `shared values` had some common cause `problem_var`, then `participant values` is a collider between `problem_var` and `demographics`; either we control for `problem_var` or we control all the demographics (and possibly worry about the exact structure of the demographics subgraph). Big problem if we didn't observe `problem_var`!  Fortunately we know by construction of `shared values` that there is no such common cause.  
#+ fig.width = 10, out.width = "100%"

dag |> 
    add_arrows(c('problem_var -> shared_values', 
                 'problem_var -> part_values')) |> 
    plot_adjustments('shared_values')

#' This doesn't happen if we have a problem variable elsewhere, because `participant values` is not a collider on any path that might result.  

dag |> 
    add_arrows(c('part_values <- problem_var -> demographics')) |> 
    plot_adjustments('shared_values')

#' So, all together, we just need to adjust for participant values and scientist values.  
model_d_emad = emad_df |> 
    filter(disclosure) %>%
    lm(pa_mean ~ shared_values + part_values + sci_values, data = .)
summary(model_d_emad)
model_d = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values + part_values + sci_values, data = .)
summary(model_d)
plot_residuals(model_d)
# plot_estimate(model_d, 'shared_values')
plot_estimate(list(emad = model_d_emad, 
                   hl = model_d), 
              str_detect(term, 'shared_values'))

#' ## H4 demographics robustness check
## H4 demographics robustness check ----
#' But we can include demographics to check the accuracy of the DAG and robustness of the estimates.  This works as expected: adding other demographics basically doesn't change the estimated effect for shared values (.10 (SE .13) vs. .07 (SE .14)). 
model_d1 = dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ shared_values + part_values + sci_values +
           age + gender + race_ethnicity + religious_affil + religious_serv + 
           political_ideology + education,
       data = .)
broom::tidy(model_d1)

#' In addition, participant values is independent of shared values: from a participant's perspective (given their values), they have an equal chance of seeing a scientist with same or different values. 
dataf |> 
    filter(disclosure) |> 
    count(part_values, sci_values, shared_values)

#' So dropping participant values from the regression doesn't change the estimated effect of shared values. 

model_d2 = dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ shared_values + sci_values, data = .)
summary(model_d2)

#' But scientist values *does* confound the estimate. 
model_d3 = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values, data = .)
summary(model_d3)


#' # Scientist values #
## Scientist values ----
#' We didn't specify this possibility in advance, but the analysis for H4 suggests scientist values, not shared values, have an effect. This is randomly assigned, so no adjustments needed. 
plot_adjustments(dag, 'sci_values') +
    scale_color_manual(values = 'black')

model_emad_s = emad_df |> 
    filter(disclosure) %>% 
    lm(pa_mean ~ sci_values, data = .)
model_s = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ sci_values, data = .)

summary(model_s)

plot_estimate(list('emad' = model_emad_s, 
                   'hl' = model_s), 
              str_detect(term, 'sci_values'))

dataf |> 
    filter(disclosure, !is.na(part_values)) |> 
    ggplot(aes(sci_values, meti_mean)) +
    # geom_violin(draw_quantiles = .5) +
    geom_beeswarm(dodge.width = 1, alpha = .25) +
    geom_ribbon(data = plot_predictions(model_s, 
                                        focal_vars = 'sci_values', 
                                        return_plot = FALSE), 
                aes(y = .fitted, ymin = .lower, ymax = .upper), 
                alpha = .25, group = 1L, fill = 'blue') +
    geom_line(data = plot_predictions(model_s, 
                                      focal_vars = 'sci_values', 
                                      return_plot = FALSE), 
              aes(y = .fitted, ymin = .lower, ymax = .upper), 
              alpha = 1, group = 1L, fill = 'blue')

#' For the paper, we'll combine all of these models into one big table
shared_values_tbl = list(univariate = tbl_regression(model_d3, 
                                                     intercept = TRUE,
                                                     label = c(shared_values ~ 'shared values')), 
                         sci_values = tbl_regression(model_d2, 
                                                     intercept = TRUE,
                                                     label = c(shared_values ~ 'shared values', 
                                                               sci_values ~ 'scientist values')),
                         part_values = tbl_regression(model_d, 
                                                      intercept = TRUE,
                                                      label = c(shared_values ~ 'shared values', 
                                                                sci_values ~ 'scientist values', 
                                                                part_values ~ 'participant values')),
                         demo = tbl_regression(model_d1, 
                                               intercept = TRUE,
                                               label = c(shared_values ~ 'shared values', 
                                                         sci_values ~ 'scientist values', 
                                                         part_values ~ 'participant values', 
                                                         religious_serv ~ 'rel. serv. attendance',
                                                         political_ideology ~ 'political id.'), 
                                               include = -c(gender, race_ethnicity, 
                                                            religious_affil)), 
                         sci_values_alone = tbl_regression(model_s, 
                                                           intercept = TRUE,
                                                           label = c(sci_values ~ 'scientist values'))) |> 
    map(add_glance_table, include = c(r.squared, nobs, adj.r.squared,
                                      statistic, p.value)) |>
    tbl_merge(tab_spanner = c('(1) univariate', 
                              '(2) scientist values',
                              '(3) participant values',
                              '(4) demographics', 
                              '(5) scientist values alone')) |> 
    modify_table_body(~ arrange(.x, row_type == "glance_statistic"))

shared_values_tbl

write_reg_tbl(shared_values_tbl, here(out_dir, '03_shared_values_tbl'))


#' # H5. Variation in effects # 
## H5. Variation in effects ----
#' The magnitude of the effects above vary depending on whether the participant prioritizes public health or economic growth.
#' 

#' ## H5-consumer: Consumer risk sensitivity ##  
## H5-consumer ----
#' For consumer risk and transparency penalty, bringing in participant values introduces a potential back-door path through demographics.  This is very similar to shared values.  Fortunately, as also with shared values, we just need to adjust for `part_values` (and `conclusion`).  

dag |> 
    add_arrows(c('part_values -> conclusion_x_part_values <- conclusion', 
                 'conclusion_x_part_values -> METI <- conclusion')) |> 
    plot_adjustments('conclusion_x_part_values')

model_eb_emad = lm(pa_mean ~ conclusion*part_values, data = emad_df)
summary(model_eb_emad)
model_eb = lm(meti_mean ~ conclusion*part_values, data = dataf)
summary(model_eb)
plot_residuals(model_eb)

plot_predictions(model_eb, c('conclusion', 'part_values'), 
                 interaction_ci = TRUE)

plot_estimate(list(base = model_b, interaction = model_eb), 
              str_detect(term, 'conclusion'))

list(base = model_b, interaction = model_eb) |> 
    reg_tbl(labs = c('base', 'interaction'))


#' Again, include demographics as a check
model_eb1 = lm(meti_mean ~ conclusion*part_values + 
                   age + gender + race_ethnicity + religious_affil + 
                   religious_serv + political_ideology + education,
               data = dataf)
summary(model_eb1)

eb_tbl = list(base = model_b, interaction = model_eb, demo = model_eb1) |> 
    reg_tbl(labs = c('base', 'interaction', 'demographics'))
eb_tbl
write_reg_tbl(eb_tbl, here(out_dir, '03_eb_tbl'))



emad_df |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(conclusion, pa_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(conclusion, meti_mean)) +
    geom_beeswarm(alpha = .5, cex = 1.5, size = .5) +
    stat_summary(fun.data = mean_cl_boot, color = 'red', 
                 size = 1, fatten = 0) +
    stat_summary(fun.data = mean_cl_boot, geom = 'line', group = 1L, color = 'red') +
    facet_wrap(vars(part_values)) +
    labs(x = 'scientist conclusion: BPA ...', 
         y = 'perceived trustworthiness')

ggsave(here(out_dir, '03_conclusion_part.png'), 
       height = 3, width = 6, scale = 1, 
       bg = 'white')

#' ## H5-transparency
## H5-transparency ----
dag |> 
    add_arrows(c('part_values -> disclosure_x_part_values <- disclosure', 
                 'disclosure_x_part_values -> METI', 
                 'disclosure -> METI')) |> 
    plot_adjustments('disclosure_x_part_values')

model_ec_emad = lm(pa_mean ~ disclosure*part_values, data = emad_df)
summary(model_ec_emad)
lm(pa_mean ~ disclosure*part_values+ 
       sex + ideology + educatio + age, data = emad_df) |> 
    summary()

model_ec = lm(meti_mean ~ disclosure*part_values, data = dataf)
summary(model_ec)
plot_residuals(model_ec)

plot_estimate(list(base = model_c, interaction = model_ec),
              str_detect(term, 'disclosure'))

plot_predictions(model_ec, c('disclosure', 'part_values'), 
                 interaction_ci = TRUE)

model_ec1 = lm(meti_mean ~ disclosure * part_values + 
                   age + gender + race_ethnicity + religious_affil + 
                   religious_serv + political_ideology + education, 
               data = dataf)

ec_tbl = list(base = model_c, 
              interaction = model_ec, 
              demographics = model_ec1) |> 
    reg_tbl(labs = c('base', 'interaction', 'demographics'))
ec_tbl
write_reg_tbl(ec_tbl, here(out_dir, '03_ec_tbl'))

dataf |> 
    filter(!is.na(part_values)) |> 
    ggplot(aes(disclosure, meti_mean)) +
    geom_boxplot() +
    # geom_beeswarm(alpha = .25) +
    facet_wrap(vars(part_values))


#' ## H5-shared
## H5-shared ----
#' Initially it seems like we can proceed as we did with the other interactions: we need to adjust for participant values and shared values, but that's all. In particular, no need to adjust for scientist values. 
dag |> 
    add_arrows(c('part_values -> shared_values_x_part_values <- shared_values', 
                 'shared_values_x_part_values -> METI')) |> 
    plot_adjustments('shared_values_x_part_values')

# model_ed_emad = emad_df |> 
#     filter(disclosure) %>%
#     lm(pa_mean ~ shared_values*part_values, data = .)
# summary(model_ed_emad)
# emad_df |> 
#     filter(disclosure) %>%
#     lm(pa_mean ~ shared_values*part_values+ 
#            sex + ideology + educatio + age, data = .) |> 
#     summary()

model_ed = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values, data = .)
summary(model_ed)

#' But that finds that shared values as a statistically significant *negative* primary effect for shared values?  That's weird. We saw above that the estimate for shared values was biased unless we included scientist values.  So let's try to include it in the model.  This results in perfect collinearity. 
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values + sci_values, data = .) |> 
    summary()

#' Comparing to our model for H4 makes it clear that this interaction model is misspecified
model_ed1 = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values+ 
           age + gender + race_ethnicity + religious_affil + 
           religious_serv + political_ideology + education, 
       data = .)
list(model_d, model_ed, model_ed1) |> 
    reg_tbl(labs = c('base', 'interaction', 'demographics'))

#' Plot mean + CI by participant and shared values.  It looks like there's an interaction here:  shared values might be positive or negative, depending on the participants' values.  
dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(shared_values, meti_mean)) +
    # geom_boxplot() +
    geom_beeswarm(alpha = .5, cex = 1.5, size = .5) +
    facet_wrap(vars(part_values)) +
    stat_summary(fun.data = mean_cl_boot, color = 'red', 
             size = 1, fatten = 0) +
    stat_summary(fun.data = mean_cl_boot, geom = 'line', group = 1L, color = 'red') +
    labs(x = 'shared values', 
         y = 'perceived trustworthiness')

ggsave(here(out_dir, '03_shared_part.png'), 
       height = 3, width = 6, scale = 1, 
       bg = 'white')

#' Could this be due to political ideology?  No: it appears to be either entirely upstream or independent to this
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values + political_ideology, data = .) |> 
    summary()

#' Apparently independent.  Interesting:  No correlation between political ideology and trust?  Very unexpected, given the narrative that trust in science is politically polarized! 
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ political_ideology, data = .) |> 
    summary()

dataf |> 
    filter(disclosure) |> 
    ggplot(aes(political_ideology, meti_mean, group = political_ideology)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

#' # Scientist values
## Scientist values ---
#' The confusing "shared values interaction" effect is just the effect of scientist values, from H4
model_es = dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ sci_values + part_values, data = .)
summary(model_es)
plot_predictions(model_es, 'sci_values')

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(sci_values, meti_mean)) +
    # geom_boxplot() +
    geom_beeswarm(alpha = .5, cex = 1.5, size = .5) +
    facet_wrap(vars(part_values)) +
    # geom_ribbon(data = plot_predictions(model_es, 
    #                                     c('sci_values', 'part_values'), 
    #                                     return_plot = FALSE), 
    #             aes(y = .fitted, ymin = .lower, ymax = .upper), 
    #             group = 1L, alpha = .5, fill = 'blue') +
    # geom_line(data = plot_predictions(model_es, 
    #                                   c('sci_values', 'part_values'), 
    #                                   return_plot = FALSE), 
    #           aes(y = .fitted, ymin = .lower, ymax = .upper), 
    #           group = 1L, alpha = 1, color = 'blue') +
    stat_summary(fun.data = mean_cl_boot, color = 'red', 
                 size = 1, fatten = 0) +
    stat_summary(fun.data = mean_cl_boot, geom = 'line', group = 1L, color = 'red') +
    labs(x = 'scientist values', 
         y = 'perceived trustworthiness')

ggsave(here(out_dir, '03_sci_part.png'), height = 3, width = 6, scale = 1, bg = 'white')


#' Any interaction with participant values is swamped by uncertainty.  
#' Though Emilio thinks this might be worth reporting because of difference in variation — wider for economic growth participants.  OTOH there are just a lot fewer of these participants, so SEs are larger.  
#' How are participants thinking of economic growth?  Might be thinking of indirect effects, eg, good economy -> better hospitals and public health system 
dataf %>% 
    filter(disclosure) %>%
    lm(meti_mean ~ sci_values*part_values, data = .) |> 
    # summary()
    plot_predictions(c('sci_values', 'part_values'), 
                     interaction_ci = TRUE)

lm(meti_mean ~ sci_values * part_values, 
   data = filter(dataf, disclosure)) |> 
    summary()

sci_part_tbl = list(base = model_s,
                    part_values = model_es,
                    interaction = lm(meti_mean ~ sci_values * part_values, 
                                     data = filter(dataf, disclosure)),
                    demographics = lm(meti_mean ~ sci_values * part_values + 
                                          age + gender + race_ethnicity + religious_affil + 
                                          religious_serv + political_ideology + education, 
                                      data = filter(dataf, disclosure))) |> 
    reg_tbl(labs = c('base', 'participant values', 
                     'interaction', 'demographics'))
write_reg_tbl(sci_part_tbl, here(out_dir, '03_sci_part_tbl'))    
