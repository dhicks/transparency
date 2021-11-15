library(tidyverse)
library(tidylog)
library(here)
library(openssl)
source(here('secrets', 'key.R'))
library(assertthat)
library(magrittr)
library(skimr)

#' # Functions #
reorderer = function(chr, ...) {
    chr |> 
        as_factor() |> 
        fct_relevel(...)
}
#' ## Turn Likert scales into factors
likert = function(chr) {
    reorderer(chr, 
              'Strongly Disagree', 
              'Disagree', 
              'Unsure', 
              'Agree', 
              'Strongly Agree')
}

#' ## True -> TRUE
is_true = function(chr) {
    chr == 'True'
}

#' # Load data #
dataf_raw = read_csv(here('data', 
                      "replication+-+scientists'+values_November+8,+2021_13.54.csv")) |> 
    slice(-1, -2) |> 
    ## Consent and completion
    filter(IC == 'I am over 18-years-old and consent to participate in this study.', 
           as.numeric(Progress) >= 99)

#' # Consolidate METI items #
meti_df = dataf_raw |> 
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


#' # Filtering and parsing #
dataf = dataf_raw |> 
    ## Consolidated METI responses
    select(-matches('METI')) |> 
    inner_join(meti_df, by = c('PROLIFIC_PID' = 'id')) |> 
    ## Identifying information
    mutate(id = sha224(PROLIFIC_PID, key = key)) |> 
    select(-IPAddress, -RecipientLastName, -RecipientFirstName, -RecipientEmail, 
           -ExternalReference, -LocationLatitude, -LocationLongitude, -PROLIFIC_PID, 
           -ResponseId, -SexAtBirth) |> 
    select(id, everything()) |> 
    ## VISS items are all Likert responses
    mutate(across(matches('ViS'), 
                  likert)) |> 
    ## SRS items are true/false
    mutate(across(matches('SRS'), 
                  is_true)) |> 
    ## Other parsing
    mutate(Age = as.integer(Age), 
           ReligiousServ = reorderer(ReligiousServ, 
                                     'Never', 
                                     'A few times per year', 
                                     'Once every month or two', 
                                     '2-3 times per month', 
                                     'Once per week', 
                                     'More than once per week', 
                                     'Daily'), 
           Education = reorderer(Education, 
                                 'Less than high school', 
                                 'High school, or some college', 
                                 'Bachelor\'s degree or higher'), 
           Values = case_when(Values == '0' ~ 'no values', 
                              Values == '1' ~ 'public health', 
                              Values == '2' ~ 'economic growth'), 
           Conclusion = case_when(Conclusion == '1' ~ 'causes harm', 
                                  Conclusion == '0' ~ 'does not cause harm'))
           


#' # Data validation #
#' ## No duplicate Prolific IDs
dataf |> 
    count(id) |> 
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
