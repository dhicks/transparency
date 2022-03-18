#' ---
#' title: "DAG analysis"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
# TODO: 
#     -[] clean code
#     -[] double check direction of coding for ideology and tradeoff in Elliott et al.
#     -[x] estimate plots showing both us + Elliott et al.
#     -[] better plots for interactions
    
    
library(tidyverse)
theme_set(theme_minimal())
library(ggbeeswarm)
library(dagitty)
library(ggdag)
## <https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html>

library(here)
source(here('R', 'plot_adjustments.R'))
source(here('R', 'reg_plots.R'))

emad_df = read_rds(here('data', 'emad.Rds'))
## VISS six-factor model
viss_df = read_csv(here('data', 'fa_six.csv')) |> 
    select(pid, starts_with('fa_'))
dataf = read_rds(here('data', 'data.Rds')) |> 
    left_join(viss_df, by = 'pid')
    # select(pid, sci_values, conclusion, disclosure, 
    #        pref, meti_mean, 
    #        SRS_sum,
    #        Age, GenderIdentity, GenderLived, `Race/Ethnicity`, 
    #        ReligiousAffil, ReligiousServ, 
    #        political_ideology, PoliticalAffiliation,
    #        Education) |> 
    # mutate(pid = as.character(pid),
    #        ## Reverse coding of METI so that increased METI -> increased trust
    #        meti_mean = 8 - meti_mean, 
    #        part_values = if_else(pref >= 3, 
    #                              'public health', 
    #                              'economic growth'), 
    #        shared_values = part_values == sci_values, 
    #        across(c(Age, ReligiousServ, political_ideology, 
    #               PoliticalAffiliation, Education), 
    #               as.integer),
    #        political_ideology = if_else(political_ideology %in% c(8, 9), 
    #                                    NA_integer_, political_ideology),
    #        gender = interaction(GenderIdentity, GenderLived), 
    #        across(c(`Race/Ethnicity`, ReligiousAffil, gender), 
    #               fct_infreq)) |> 
    

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

#' Across our dataset, standard deviation of mean trust.  Use one-third of this as meaningful. 
sd(dataf$meti_mean)
meaningful = sd(dataf$meti_mean)/3
meaningful

## A. Modest correlation between values and ideology ----
#' # A. Modest correlation between values and ideology #
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

dataf |> 
    filter(!is.na(pref)) |> 
    count(political_ideology, pref) |> 
    group_by(political_ideology) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    ggplot(aes(political_ideology, n, fill = pref)) +
    geom_col() +
    scale_fill_viridis_d()

last_plot() + aes(y = share)

table(dataf$political_ideology, dataf$pref)

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

## DAG ----
#' # DAG #
#' We use the following DAG throughout the rest of this analysis
dag = dagify(METI ~ viss +
                 shared_values + sci_values +
                 part_values + demographics,
             shared_values ~ part_values + sci_values, 
             part_values ~ demographics,
             viss ~ demographics + part_values, 
             outcome = 'METI') |> 
    tidy_dagitty(layout = 'kk')

ggplot(dag, aes(x = x, y = y, 
                xend = xend, yend = yend)) +
    geom_label(aes(label = name)) +
    geom_dag_edges() +
    theme_dag()


## B. Consumer risk sensitivity ----
#' # B. Consumer risk sensitivity #
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


## C. Transparency penalty ----
#' # C. Transparency penalty #
#' *Scientists who disclose values are perceived as less trustworthy than scientists who do not.* 

ggplot(emad_df, aes(disclosure, pa_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

ggplot(dataf, aes(disclosure, meti_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()


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



## D. Shared values ----
#' # D. Shared values #
#' *Given that the scientist discloses values, if the participant and the scientist share the same values, the scientist is perceived as more trustworthy than if the participant and scientist have discordant values.*

ggplot(emad_df, aes(shared_values, pa_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

ggplot(dataf, aes(shared_values, meti_mean)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()


#' In our actual situation, we only need to adjust for `participant values` and `scientist values`; `participant values` is on every back-door path running through demographics or VISS variables.  

plot_adjustments(dag, 'shared_values')

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
    lm(pa_mean ~ shared_values + part_values + sci_values, data = .) |> 
    summary()
model_d = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values + part_values + sci_values, data = .)
summary(model_d)
plot_residuals(model_d)
# plot_estimate(model_d, 'shared_values')
plot_estimate(list(emad = model_d_emad, 
                   hl = model_d), 
              str_detect(term, 'shared_values'))

#' But we can include demographics to check the accuracy of the graph.  In the Elliott et al. data, this holds true:  adding other demographics doesn't change the estimated effect for shared values of 0.4.  
#' TODO: this for our data
dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ shared_values + part_values + sci_values +
           age + gender + race_ethnicity + religious_affil + religious_serv + 
           political_ideology + education,
       data = .) |> 
    broom::tidy()

#' And actually participant values is independent of shared values: from a participant's perspective (given their values), they have an equal chance of seeing a scientist with same or different values. 
dataf |> 
    filter(disclosure) |> 
    count(part_values, sci_values, shared_values)

#' So dropping participant values from the regression doesn't change the estimated effect of shared values. 

dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ shared_values + sci_values, data = .) |> 
    summary()

#' But scientist values *does* confound the estimate. 
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values, data = .) |> 
    summary()

#' ## Scientist values ##
#' We didn't specify this possibility in advance, but all this suggests scientist values, not shared values, have an effect. This is randomly assigned, so no adjustments needed. 
plot_adjustments(dag, 'sci_values') +
    scale_color_manual(values = 'black')

dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ sci_values, data = .) |> 
    summary()


## E. Variation in effects ----
#' # E. Variation in effects # 
#' The magnitude of the effects above vary depending on whether the participant prioritizes public health or economic growth.
#' 

#' ## B: Consumer risk sensitivity ##  
#' For B and C, bringing in participant values introduces a potential back-door path through demographics.  This is very similar to D.  Fortunately, as also with D, we just need to control `part_values` (and `conclusion`).  

dag |> 
    add_arrows(c('part_values -> conclusion_x_part_values <- conclusion', 
                 'conclusion_x_part_values -> METI <- conclusion')) |> 
    plot_adjustments('conclusion_x_part_values')

model_eb_emad = lm(pa_mean ~ conclusion*part_values, data = emad_df)
summary(model_eb_emad)
model_eb = lm(meti_mean ~ conclusion*part_values, data = dataf)
summary(model_eb)
plot_residuals(model_eb)
# plot_estimate(model_eb, ':')
plot_estimate(list(emad = model_eb_emad, 
                   hl = model_eb), 
              str_detect(term, ':'))

#' Again, include demographics as a check.  
#' TODO
lm(pa_mean ~ conclusion*part_values+ 
       sex + ideology + educatio + age, 
   data = emad_df) |> 
    summary()

emad_df |> 
    filter(!is.na(part_values)) |> 
    ggplot(aes(conclusion, pa_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))


#' ## C: Disclosure ##
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
# plot_estimate(model_ec, ':')
plot_estimate(list(emad = model_ec_emad, 
                   hl = model_ec), 
              str_detect(term, ':'))

emad_df |> 
    filter(!is.na(part_values)) |> 
    ggplot(aes(disclosure, pa_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))

#' ## D: Shared values ##
dag |> 
    add_arrows(c('part_values -> shared_values_x_part_values <- shared_values', 
                 'shared_values_x_part_values -> METI')) |> 
    plot_adjustments('shared_values_x_part_values')

model_ed_emad = emad_df |> 
    filter(disclosure) %>%
    lm(pa_mean ~ shared_values*part_values, data = .)
summary(model_ed_emad)
emad_df |> 
    filter(disclosure) %>%
    lm(pa_mean ~ shared_values*part_values+ 
           sex + ideology + educatio + age, data = .) |> 
    summary()

model_ed = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values, data = .)
summary(model_ed)

plot_estimate(list(emad = model_ed_emad, 
                   hl = model_ed), 
              str_detect(term, ':'))

dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(shared_values, meti_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))

#' And political ideology appears to be either entirely upstream or independent to this
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ shared_values*part_values + political_ideology, data = .) |> 
    summary()

#' Apparently independent
dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ political_ideology, data = .) |> 
    summary()

dataf |> 
    filter(disclosure) |> 
    ggplot(aes(political_ideology, meti_mean, group = political_ideology)) +
    geom_violin(draw_quantiles = .5) +
    geom_beeswarm()

#' Because the effect is with *scientist* values (we also saw this at the end of D)
dataf |> 
    filter(!is.na(part_values), disclosure) |> 
    ggplot(aes(sci_values, meti_mean)) +
    geom_boxplot() +
    facet_wrap(vars(part_values))

dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ sci_values, data = .) |> 
    summary()

#' Any interaction with participant values is swamped by uncertainty
dataf %>% 
    filter(disclosure) %>%
    lm(meti_mean ~ sci_values*part_values, data = .) |> 
    summary()


## F. VISS ----
#' # F. VISS #
#' *Effects c-e will vary depending the participant's views of the role of values in science (the latent variables from the six-factor model)*


#' ## VISS and demographics ##
#' Continuous and ordinal demographics
dataf |> 
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    ggpubr::stat_cor(label.x.npc = 'center', label.y.npc = 'center', 
                     geom = 'rect', alpha = .8,
                     mapping = aes(fill = after_stat(r), 
                                   x = .panel_x, y = .panel_y,
                                   xmin = -Inf, xmax = Inf, 
                                   ymin = -Inf, ymax = Inf)) +
    geom_jitter(alpha = .1) +
    geom_smooth(method = 'lm') +
    ggpubr::stat_cor(label.x.npc = 'right', label.y.npc = 'bottom', 
                     geom = 'label', hjust = 'right', vjust = 'bottom',
                     size = 5, 
                     r.accuracy = .01,
                     mapping = aes(label = after_stat(r.label), 
                                   fill = after_stat(r))) +
    ggforce::facet_matrix(rows = vars(starts_with('fa_')),
                          cols = vars(age, religious_serv, political_ideology, 
                                      political_affiliation, education), 
                          switch = 'y') +
    scale_fill_gradient2(limits = c(-1, 1)) +
    theme_bw()

#' Categorical demographics
dataf |> 
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_boxplot(aes(group = .panel_y), varwidth = TRUE, 
                 orientation = NA) +
    ggforce::facet_matrix(cols = vars(starts_with('fa_')),
                          rows = vars(race_ethnicity, gender,
                                      religious_affil,
                                      part_values), 
                          switch = 'y') +
    theme_bw()


#' ## VISS effects by themselves ##
#' Will be difficult to disentangle from demographics
dag |> 
    plot_adjustments('viss')

lm(meti_mean ~ fa_scientism + fa_vis + fa_cynicism +
                   fa_power + fa_textbook + fa_vfi +
           age + gender + race_ethnicity + religious_affil + religious_serv +
           political_ideology + political_affiliation + education, 
   data = dataf) |> 
    list() |> 
    plot_estimate(str_detect(term, 'fa_'))
    # broom::tidy(conf.int = TRUE) |>
    # filter(str_detect(term, 'fa_')) |>
    # ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
    # geom_pointrange() +
    # geom_hline(yintercept = c(meaningful, -meaningful), linetype = 'dashed') +
    # coord_flip()

#' ## B (Consumer Risk) and C (Disclosure) ##
dag |> 
    add_arrows(c('conclusion -> conclusion_x_viss <- viss', 
                 'conclusion_x_viss -> METI <- conclusion')) |> 
    plot_adjustments('conclusion_x_viss')

lm(meti_mean ~ conclusion*fa_scientism + 
       conclusion*fa_vis + 
       conclusion*fa_cynicism + 
       conclusion*fa_power + 
       conclusion*fa_textbook + 
       conclusion*fa_vfi, 
   data = dataf) |> 
    summary()
    # list() |>
    # plot_estimate(str_detect(term, 'fa_'))

dag |> 
    add_arrows(c('disclosure -> disclosure_x_viss <- viss', 
                 'disclosure_x_viss -> METI', 
                 'disclosure -> METI')) |> 
    plot_adjustments('disclosure_x_viss')

lm(meti_mean ~ disclosure*fa_scientism + 
               disclosure*fa_vis + 
               disclosure*fa_cynicism + 
               disclosure*fa_power + 
               disclosure*fa_textbook + 
               disclosure*fa_vfi, 
   data = dataf) |> 
    summary()

#' ## D: Shared values ##
dag |> 
    add_arrows(c('shared_values -> shared_values_x_viss <- viss', 
                 'shared_values_x_viss -> METI')) |> 
    plot_adjustments('shared_values_x_viss')

dataf |> 
    filter(disclosure) %>% 
    lm(meti_mean ~ shared_values*fa_scientism + 
                   shared_values*fa_vis + 
                   shared_values*fa_cynicism + 
                   shared_values*fa_power + 
                   shared_values*fa_textbook + 
                   shared_values*fa_vfi, 
   data = .) |> 
    summary()

#' In D, we saw that the expected shared values effect is better conceived as a scientist values effect.
dag |> 
    add_arrows(c('sci_values -> sci_values_x_viss <- viss', 
                 'sci_values_x_viss -> METI')) |> 
    plot_adjustments('sci_values_x_viss')

model_fd = dataf |> 
    filter(disclosure) %>%
    lm(meti_mean ~ sci_values*fa_scientism + 
                   sci_values*fa_vis + 
                   sci_values*fa_cynicism + 
                   sci_values*fa_power + 
                   sci_values*fa_textbook + 
                   sci_values*fa_vfi, 
       data = .)
summary(model_fd)

cross_df(list(sci_values = c('public health', 'economic growth'), 
              fa_scientism = 3, 
              fa_vis = 3, 
              fa_cynicism = seq(1, 5, by = .1),
              fa_power = 3, 
              fa_textbook = 3, 
              fa_vfi = 3)) %>%
    broom::augment(model_fd, newdata = ., interval = 'confidence') |> 
    ggplot(aes(fa_cynicism, .fitted, group = sci_values, 
               color = sci_values, fill = sci_values)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .25) +
    geom_line()

dataf |> 
    filter(disclosure) |> 
    ggplot(aes(fa_cynicism, meti_mean, color = sci_values)) +
    geom_point() +
    stat_smooth(method = lm)

