library(tidyverse)
library(tidylog)
library(here)
library(openssl)
source(here('secrets', 'key.R'))

library(assertthat)
library(magrittr)
library(skimr)
library(glue)

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

#' #' ## True -> TRUE
#' is_true = function(chr) {
#'     chr == 'True'
#' }

#' # Load data #
# full survey + pre_clean
dataf_raw_survey <- read_csv(here('data', "full_replication_scientists_values.csv"))

set.seed(2021-12-15)
d_depersonalized_survey <- dataf_raw_survey |> 
  slice(-1,-2) |> 
  filter(IC == '1', as.numeric(Progress) >= 99) |> 
  ## Depersonalize
  mutate(pid = hash(PROLIFIC_PID)) |> 
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
  mutate(Disclosure = Values != '0', 
         Values = if_else(Disclosure, 
                          Values, 
                          sample(c('1', '2'), nrow(.), replace = TRUE)), 
         Values = case_when(Values == '1' ~ 'public health',
                            Values == '2' ~ 'economic growth')) |> 
  ## Convert
  mutate(Age = as.integer(Age), 
         Conclusion = case_when(Conclusion == '1' ~ 'causes harm',
                                Conclusion == '0' ~ 'does not cause harm'), 
         across(c(GenderIdentity, GenderLived), 
                convert_gender))

d_depersonalized_survey

# preference question + pre_clean
dataf_raw_preference <- read_csv(here('data', "replication_preference.csv")) 
d_depersonalized_pref <- dataf_raw_preference |> 
  slice(-1,-2) |> 
  filter(IC == '1') |> 
  mutate(pid = hash(PROLIFIC_PID)) |> 
  select(-c(StartDate, 
            EndDate, 
            Status, 
            IPAddress, #def needs to be removed before being shared
            `Duration (in seconds)`,
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
            `Create New Field or Choose From Dropdown...`, 
            PROLIFIC_PID))


#' # Consolidate METI items #
meti_df = d_depersonalized_survey |> 
  select(pid, matches('METI')) |> 
  pivot_longer(-pid, 
               names_to = 'METI', 
               values_to = 'response') |> 
  filter(!is.na(response)) |> 
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

#' ## Merge ##
dataf <- full_join(d_depersonalized_survey, d_depersonalized_pref, by = "pid") |> 
  select(-matches('METI')) |> 
  full_join(meti_df, by = 'pid') |> 
  drop_na(METI01:METI14) |> 
  drop_na(SRS01:SRS11) |> 
  select(-matches('.x', '.y'))


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
    pull(Education) |>
    is.na() |>
    not() |>
    all() |>
    assert_that(msg = 'Missing values for Education')

#' ## No missing values for Values or Conclusion
dataf |>
    pull(Values, Conclusion) |>
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
