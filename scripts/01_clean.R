library(tidyverse)
library(tidylog)
library(here)
library(openssl)
source(here('secrets', 'key.R'))
library(assertthat)
library(magrittr)
library(skimr)

#' #' # Functions #
#' reorderer = function(chr, ...) {
#'     chr |> 
#'         as_factor() |> 
#'         fct_relevel(...)
#' }
#' #' ## Turn Likert scales into factors
#' likert = function(chr) {
#'     reorderer(chr, 
#'               'Strongly Disagree', 
#'               'Disagree', 
#'               'Unsure', 
#'               'Agree', 
#'               'Strongly Agree')
#' }
#' 
#' #' ## True -> TRUE
#' is_true = function(chr) {
#'     chr == 'True'
#' }

#' # Load data #

# full survey + pre_clean
dataf_raw_survey <- read_csv(here('data', "full_replication_scientists_values.csv"))

dataf_raw_survey <- dataf_raw_survey |> 
  slice(-1,-2) |> 
  filter(IC == '1', as.numeric(Progress) >=99)

d_depersonalized_survey <- subset(dataf_raw_survey, select = -c(StartDate, 
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
                                                                UserLanguage))

# preference question + pre_clean
dataf_raw_preference <- read_csv(here('data', "replication_preference.csv"))

dataf_raw_preference <- dataf_raw_preference |> 
  slice(-1,-2) |> 
  filter(IC == '1')

d_depersonalized_pref <- subset(dataf_raw_preference, select = -c(StartDate, 
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
                                                                  `Create New Field or Choose From Dropdown...`))

# merge
dataf_full <- merge(d_depersonalized_survey, d_depersonalized_pref, by = c("PROLIFIC_PID", "IC", "Progress", "Finished"), all.x = TRUE, all.y = TRUE)

#' # Consolidate METI items #
meti_df = dataf_full |> 
    rename(id = PROLIFIC_PID) |> 
    select(id, matches('METI')) |> 
    pivot_longer(-id, 
                 names_to = 'METI', 
                 values_to = 'response') |> 
    filter(!is.na(response)) |> 
    separate(METI, into = c('group', 'item')) |> 
    mutate(group = str_remove(group, 'METI')) |> 
    mutate(METI = {item %>% 
            str_pad(2, pad = '0') %>% 
            str_c('METI', .)}, 
           response = as.integer(response)) |> 
    select(id, METI, response) |> 
    pivot_wider(names_from = METI, values_from = response, values_fill = NA)

# merge consolidated METI items and drop Ps with missing
dataf = dataf_full |>
    ## Consolidated METI responses
    select(-matches('METI')) |>
    inner_join(meti_df, by = c('PROLIFIC_PID' = 'id')) |> 
  drop_na(METI01:METI14) |> 
  select(-PROLIFIC_PID)

# create METI mean scale and subscale columns
dataf$meti_mean <- rowMeans(subset(dataf, select = c(METI01:METI14)), na.rm = TRUE)
dataf$meti_competence <- rowMeans(subset(dataf, select = c(METI01:METI06)), na.rm = TRUE)
dataf$meti_integrity <- rowMeans(subset(dataf, select = c(METI07:METI10)), na.rm = TRUE)
dataf$meti_benevolence <- rowMeans(subset(dataf, select = c(METI11:METI14)), na.rm = TRUE)

# rescore SRS items, 0 = incorrect; 1 = correct
dataf <- dataf |>  
  mutate_at(c("SRS01","SRS03","SRS07"), 
            funs(recode(., `1`=0, `2`=1, .default = NaN))) |> 
  mutate_at(c("SRS02","SRS04","SRS05","SRS06","SRS08","SRS09","SRS10","SRS11"), 
            funs(recode(., `1`=1, `2`=0, .default = NaN)))

# dropping Ps with missing SRS response item values
dataf <- dataf |> 
  drop_na(SRS01:SRS11)

# sum correct SRS items
dataf$SRS_sum <- rowSums(subset(dataf, select = c(SRS01:SRS11)), na.rm = TRUE)


#' #' # Filtering and parsing #
#' dataf = dataf_raw |> 
#'     ## Consolidated METI responses
#'     select(-matches('METI')) |> 
#'     inner_join(meti_df, by = c('PROLIFIC_PID' = 'id')) |> 
#'     ## Identifying information
#'     mutate(id = sha224(PROLIFIC_PID, key = key)) |> 
#'     select(-IPAddress, -RecipientLastName, -RecipientFirstName, -RecipientEmail, 
#'            -ExternalReference, -LocationLatitude, -LocationLongitude, -PROLIFIC_PID, 
#'            -ResponseId, -SexAtBirth) |> 
#'     select(id, everything()) |> 
#'     ## VISS items are all Likert responses
#'     mutate(across(matches('ViS'), 
#'                   likert)) |> 
#'     ## SRS items are true/false
#'     mutate(across(matches('SRS'), 
#'                   is_true)) |> 
#'     ## Other parsing
#'     mutate(Age = as.integer(Age), 
#'            ReligiousServ = reorderer(ReligiousServ, 
#'                                      'Never', 
#'                                      'A few times per year', 
#'                                      'Once every month or two', 
#'                                      '2-3 times per month', 
#'                                      'Once per week', 
#'                                      'More than once per week', 
#'                                      'Daily'), 
#'            Education = reorderer(Education, 
#'                                  'Less than high school', 
#'                                  'High school, or some college', 
#'                                  'Bachelor\'s degree or higher'), 
#'            Values = case_when(Values == '0' ~ 'no values', 
#'                               Values == '1' ~ 'public health', 
#'                               Values == '2' ~ 'economic growth'), 
#'            Conclusion = case_when(Conclusion == '1' ~ 'causes harm', 
#'                                   Conclusion == '0' ~ 'does not cause harm'))
#'            
#' 
#' 
#' #' # Data validation #
#' #' ## No duplicate Prolific IDs
#' dataf |> 
#'     count(id) |> 
#'     filter(n > 1) |> 
#'     nrow() |> 
#'     identical(0L) |> 
#'     assert_that(msg = 'Duplicate Prolific IDs')
#' 
#' #' ## Non-missing values for education
#' #' This was the final demographic question; non-missing value indicates completion
#' dataf |> 
#'     pull(Education) |> 
#'     is.na() |> 
#'     not() |> 
#'     all() |> 
#'     assert_that(msg = 'Missing values for Education')
#' 
#' #' ## No missing values for Values or Conclusion
#' dataf |> 
#'     pull(Values, Conclusion) |> 
#'     is.na() |> 
#'     not() |> 
#'     all() |> 
#'     assert_that(msg = 'Missing values in Values or Conclusion')

#' # Skim #
skim(dataf)

#' Note that `skim()` only shows the 4 most common values for factors, in descending order
count(dataf, ViS36)

#' # Write out #
write_csv(dataf, here('data', 'data.csv'))
write_rds(dataf, here('data', 'data.Rds'))
