#' ---
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

library(tidyverse)
library(tidylog)
library(here)
library(openssl)
source(here('secrets', 'key.R'))

library(assertthat)
library(magrittr)
library(skimr)
library(glue)
library(snakecase)

#' # Functions #
hash = function(pid) {
    sha224(pid, key = key)
}

convert_gender = function(resp) {
    resp |> 
        str_split(',') |> 
        map(~ case_when(.x == '1' ~ 'Woman/Female',
                        .x == '2' ~ 'Man/Male',
                        .x == '3' ~ 'Indigenous or other cultural gender minority identity',
                        .x == '4' ~ 'Genderqueer/Gender non-binary/Gender fluid',
                        .x == '5' ~ 'Different Identity')) |> 
        map_chr(str_c, collapse = ' & ')
}

# ## True -> TRUE
# is_true = function(chr) {
#     chr == 'True'
# }


## load data ----
#' # Load data #
# full survey + pre_clean
dataf_raw_survey <- read_csv(here('data', "full_replication_scientists_values.csv"))

set.seed(2021-12-15)
d_depersonalized_survey <- dataf_raw_survey |> 
    slice(-1,-2) |> 
    filter(IC == '1', as.numeric(Progress) >= 99) |> 
    ## Depersonalize
    mutate(pid = hash(PROLIFIC_PID), 
           pid = as.character(pid)) |> 
    select(-c(StartDate, 
              EndDate, 
              Status, 
              IPAddress, #def needs to be removed before being shared
              RecordedDate,
              ResponseId, 
              RecipientLastName,
              RecipientFirstName,
              RecipientEmail,
              ExternalReference,
              LocationLatitude, #def needs to be removed before being shared
              LocationLongitude, #def needs to be removed before being shared
              DistributionChannel,
              UserLanguage, 
              PROLIFIC_PID, 
              SexAtBirth)) |> 
    ## rescore SRS items, 0 = incorrect; 1 = correct
    mutate_at(c("SRS01","SRS03","SRS07"), 
              funs(recode(., `1`=0, `2`=1, .default = NaN))) |> 
    mutate_at(c("SRS02","SRS04","SRS05","SRS06","SRS08","SRS09","SRS10","SRS11"), 
              funs(recode(., `1`=1, `2`=0, .default = NaN))) |> 
    rowwise() |> 
    mutate(SRS_sum = mean(c_across(SRS01:SRS11), na.rm = TRUE)) |> 
    ungroup() %>%
    ## Make values disclosure independent of scientist's values
    rename(sci_values = Values) %>% 
    mutate(disclosure = sci_values != '0', 
           sci_values = if_else(disclosure, 
                                sci_values, 
                                sample(c('1', '2'), nrow(.), replace = TRUE)), 
           sci_values = case_when(sci_values == '1' ~ 'public health',
                                  sci_values == '2' ~ 'economic growth')) |> 
    ## Change case
    rename_with(snakecase::to_snake_case, !c(starts_with('SR'), 
                                             starts_with('ViS'), 
                                             starts_with('METI'))) |> 
    ## Recode various things
    mutate(conclusion = case_when(conclusion == '1' ~ 'causes harm',
                                  conclusion == '0' ~ 'does not cause harm'), 
           across(c(gender_identity, gender_lived), 
                  convert_gender), 
           gender = interaction(gender_identity, gender_lived), 
           across(c(race_ethnicity, religious_affil, gender), 
                  fct_infreq),
           across(c(age, 
                    religious_serv, 
                    political_ideology, 
                    political_affiliation, 
                    education, 
                    starts_with('ViS'), 
                    starts_with('METI')), 
                  as.integer), 
           political_ideology = if_else(political_ideology %in% c(8, 9), 
                                        NA_integer_, 
                                        political_ideology))

# skim(d_depersonalized_survey)

## preference question ----
#' # Preference question #
dataf_raw_preference <- read_csv(here('data', "replication_preference.csv")) 
d_depersonalized_pref <- dataf_raw_preference |> 
    slice(-1,-2) |> 
    filter(IC == '1') |> 
    mutate(pid = hash(PROLIFIC_PID), 
           pid = as.character(pid)) |> 
    select(pid, pref) |> 
    mutate(part_values = if_else(pref >= 3, 
                                 'public health', 
                                 'economic growth'))

## METI items ----
#' # Consolidate METI items #
meti_df = d_depersonalized_survey |> 
    select(pid, matches('METI')) |> 
    pivot_longer(-pid, 
                 names_to = 'METI', 
                 values_to = 'response') |> 
    filter(!is.na(response)) |> 
    ## Reverse coding: higher -> greater trust
    mutate(response = 8 - response) |> 
    separate(METI, into = c('group', 'item')) |> 
    mutate(group = str_remove(group, 'METI')) |> 
    mutate(METI = {item %>% 
            str_pad(2, pad = '0') %>% 
            str_c('METI', .)}, 
           response = as.integer(response)) |> 
    select(pid, METI, response) |> 
    pivot_wider(names_from = METI, values_from = response, values_fill = NA) |> 
    rowwise() |> 
    mutate(meti_mean =        mean(c_across(METI01:METI14)), 
           meti_competence =  mean(c_across(METI01:METI06)), 
           meti_integrity =   mean(c_across(METI07:METI10)), 
           meti_benevolence = mean(c_across(METI11:METI14))) |> 
    ungroup()


## merge ----
#' # Merge #
dataf <- full_join(d_depersonalized_survey, d_depersonalized_pref, by = "pid") |> 
    mutate(shared_values = part_values == sci_values) |> 
    select(-matches('METI')) |> 
    full_join(meti_df, by = 'pid') |> 
    drop_na(METI01:METI14) |> 
    drop_na(SRS01:SRS11) |> 
    select(-matches('.x', '.y'))


## data validation ----
#' # Data validation #
#' ## No duplicate Prolific IDs
dataf |>
    count(pid) |>
    filter(n > 1) |>
    nrow() |>
    identical(0L) |>
    assert_that(msg = 'Duplicate Prolific IDs')

#' ## Non-missing values for education
#' This was the final demographic question; non-missing value indicates completion
dataf |>
    pull(education) |>
    is.na() |>
    not() |>
    all() |>
    assert_that(msg = 'Missing values for Education')

#' ## No missing values for sci_values or conclusion
dataf |>
    pull(sci_values, conclusion) |>
    is.na() |>
    not() |>
    all() |>
    assert_that(msg = 'Missing values in Values or Conclusion')

#' # Skim #
skim(dataf)

#' Note that `skim()` only shows the 4 most common values for factors, in descending order
count(dataf, ViS36)

#' # Write out #
write_csv(dataf, here('data', 'data.csv'))
write_rds(dataf, here('data', 'data.Rds'))


#' # Elliott et al. data #
#' The data collected by Elliott et al. is similar to ours.  Two important differences: 
#' 
#' 1. For their DV, they use a 7-point semantic difference scale that partially overlaps with METI.  *: item also in METI. 
#'     - competent*
#'     - credible
#'     - expert
#'     - honest*
#'     - intelligent*
#'     - sincere*
#'     - trustworthy
#'     
#'    They don't include a codebook, but I think the `sd` items correspond to these, and `sdind` to their mean. 
#' 2. They collect fewer demographic variables, and generally structure them differently. 
#'     - age: Binned, values 1-10
#'     - sex: Dichotomous
#'     - ideology: 1 = "very conservative" to 7 = "very liberal"
#'     - education: 1 = "up to high school diploma or equivalent" to 4 = "graduate/professional degree"

emad_xwalk = tribble(
    ~ condition, ~ disclosure,     ~ sci_values,          ~ conclusion, 
    'conditio',        FALSE,                NA,         'causes harm',
    'conditi0',        FALSE,                NA, 'does not cause harm',
    'conditi1',         TRUE,   'public health',         'causes harm',
    'conditi2',         TRUE,   'public health', 'does not cause harm',
    'conditi3',         TRUE, 'economic growth',         'causes harm',
    'conditi4',         TRUE, 'economic growth', 'does not cause harm'
)

## Elliott, McCright, Allen, and Dietz
emad = read_csv(here('data', 'elliot_et_al', 'Values and Science Experiment 1.csv'))

emad_clean = emad |> 
    ## Condition should be 1 variable, not 6
    pivot_longer(conditio:conditi4, 
                 names_to = 'condition', 
                 values_to = 'condition_indicator') |> 
    filter(condition_indicator == 1) |> 
    select(-condition_indicator) |> 
    ## Disclosure, Values, Conclusion
    left_join(emad_xwalk, by = 'condition') %>% 
    ## Disclosure _||_ Values
    mutate(sci_values = if_else(disclosure, 
                                sci_values, 
                            sample(c('public health', 'economic growth'), 
                                   nrow(.), 
                                   replace = TRUE))) |> 
    ## I'm guessing that `sd` variables are positive affect, and `sdind` appears to be their mean
    select(ourid, sci_values, conclusion, disclosure, 
           starts_with('sd'), 
           pa_mean = sdind, 
           tradeoff,
           ## NB demographics are generally handled differently
           age, sex, ideology, educatio) |> 
    rename_with(~ str_replace(., 'sd', 'pa_'), starts_with('sd')) |> 
    mutate(part_values = if_else(tradeoff >= 3, 
                                 'public health', 
                                 'economic growth'), 
           shared_values = sci_values == part_values)

assert_that(identical(nrow(emad), nrow(emad_clean)))

write_csv(emad_clean, here('data', 'emad.csv'))
write_rds(emad_clean, here('data', 'emad.Rds'))
