library(tidyverse)
library(here)
library(skimr)
library(visdat)
library(assertthat)

dataf = read_csv(here('data', 
                      "replication - scientists' values_November 6, 2021_15.07.csv")) |> 
    slice(-1, -2)


skim(dataf)

## All respondents gave consent
dataf |> 
    pull(IC) |> 
    magrittr::equals('I am over 18-years-old and consent to participate in this study.') |> 
    all() |> 
    assert_that(msg = 'Not all participants gave consent')

## Free responses to GenderIdentity_5_TEXT, Race/Ethnicity_6_TEXT, ReligiousAffil_8_TEXT,  PoliticalIdeology_8_TEXT, PoliticalAffiliation_4_TEXT
count(dataf, GenderIdentity_5_TEXT)
count(dataf, `Race/Ethnicity_6_TEXT`)
count(dataf, ReligiousAffil_8_TEXT)
count(dataf, PoliticalIdeology_8_TEXT)
count(dataf, PoliticalAffiliation_4_TEXT)



## 4 Prolific IDs that appear twice
## Because they completed the survey later
dataf |> 
    count(PROLIFIC_PID) |> 
    filter(n > 1)
dataf |> 
    count(PROLIFIC_PID) |> 
    filter(n > 1) |> 
    inner_join(dataf, by = 'PROLIFIC_PID') |> 
    count(PROLIFIC_PID, Finished)

## 1016 finished
## 11 more at 99
dataf |> 
    filter(Finished == 'True')
dataf |> 
    filter(Finished == 'False') |> 
    count(Progress)
## 1027 with Progress >= 99
dataf |> 
    mutate(Progress = as.numeric(Progress)) |> 
    filter(Progress >= 99)

## After filtering by Progress >= 99, no duplicates by PROLIFIC_PID
dataf |> 
    mutate(Progress = as.numeric(Progress)) |> 
    filter(Progress >= 99) |> 
    count(PROLIFIC_PID) |> 
    filter(n > 1) |> 
    nrow() |> 
    identical(0L) |> 
    assert_that(msg = 'Duplicate Prolific IDs')

## Prolific metadata file ----
prolific = read_csv(here('data', 'prolific_export_616d89718b4e05e5c58964b5.csv'))

## No duplicates
prolific |> 
    count(participant_id) |> 
    filter(n > 1)

## Match to Prolific data file using 'PROLIFIC_PID' = 'participant_id'
## 1027 matches
comb = inner_join(dataf, prolific, by = c('PROLIFIC_PID' = 'participant_id'))

## 21 pilot responses on October 1 that aren't in the Prolific metadata file
anti_join(dataf, prolific, by = c('PROLIFIC_PID' = 'participant_id')) |> 
    pull(EndDate) 

## 8 responses in the Prolific metadata but not survey responses
## 7 were returned or timed out
## 602d7f78c32cfe7ec12a4c64 is NOCODE, so presumably I manually approved, in principle after confirming their data was in Qualtrics *shrug*
anti_join(prolific, dataf, by = c('participant_id' = 'PROLIFIC_PID')) |> 
    view()

## Same 4 duplicates after joining; removed by filtering Progress >= 99
comb |> 
    count(PROLIFIC_PID) |> 
    filter(n > 1)
comb |> 
    mutate(Progress = as.numeric(Progress)) |> 
    filter(Progress >= 99) |> 
    count(PROLIFIC_PID) |> 
    filter(n > 1)
## 1006 responses
comb |> 
    mutate(Progress = as.numeric(Progress)) |> 
    filter(Progress >= 99)


## Missing values ----
## Look okay
vis_miss(dataf)
dataf |> 
    mutate(Progress = as.numeric(Progress)) |> 
    filter(Progress >= 99) |> 
    vis_miss(cluster = TRUE)


## Demographics ----
count(comb, GenderIdentity)
count(comb, GenderLived)

## Combining with the Prolific data is so interesting!  
count(comb, GenderIdentity, GenderLived, Sex) |> view()

## Using just the survey responses
## 4 values: GC (gender-conforming) woman, GC man, GNC, NA
## NB NAs drop out after filtering by Progress
dataf |> 
    filter(as.numeric(Progress) >= 99) |> 
    count(GenderIdentity, GenderLived) |> 
    view()

dataf |> 
    filter(as.numeric(Progress) >= 99) |> 
    mutate(gender = case_when(GenderIdentity == 'Woman/Female' & (GenderLived == 'Woman/Female' | is.na(GenderLived)) ~ 'GC woman', 
                              GenderIdentity == 'Man/Male' & (GenderLived == 'Man/Male' | is.na(GenderLived)) ~ 'GC man', 
                              is.na(GenderIdentity) & is.na(GenderLived) ~ 'missing', 
                              TRUE ~ 'GNC')) |> 
    count(gender) |> 
    mutate(share = n/sum(n))


## VISS
likert = function(chr) {
    chr |> 
        as_factor() |> 
        fct_relevel('Strongly Disagree', 
                    'Disagree', 
                    'Unsure', 
                    'Agree', 
                    'Strongly Agree')
}
cor_df = dataf |> 
    filter(as.numeric(Progress) >= 99) |> 
    select(matches('ViS')) |>
    # select(ViS01) |> 
    mutate(across(everything(), 
                  likert)) |> 
    mutate(across(everything(),
                  as.integer)) %>%
    filter(complete.cases(.)) |> 
    cor(method = 'spearman') |> 
    as_tibble(rownames = 'var1') |> 
    pivot_longer(-var1, 
                 names_to = 'var2', 
                 values_to = 'corr')

ggplot(cor_df, aes(var1, var2, fill = corr)) +
    geom_tile() +
    scale_fill_gradient2()

dataf |> 
    select(matches('ViS')) %>%
    filter(complete.cases(.)) |> 
    mutate(across(matches('ViS'), 
                  likert)) %>%
    ggplot(aes(ViS01, ViS02)) +
    # geom_count(na.rm = TRUE)
    geom_point(position = 'jitter')

dataf |> 
    mutate(across(matches('ViS'), 
                  likert)) |> 
    ggplot(aes(ViS01)) +
    geom_bar()

## Stacked bar plot for each item
dataf |> 
    select(matches('ViS')) |> 
    mutate(across(matches('ViS'), 
                  likert)) |> 
    pivot_longer(everything(), 
                 names_to = 'ViS', 
                 values_to = 'response') |> 
    count(ViS, response) |> 
    ggplot(aes(ViS, n, fill = response)) +
    geom_col(position = 'stack') +
    coord_flip() +
    scale_fill_viridis_d()

## Highest-entropy items
dataf |> 
    select(matches('ViS')) |> 
    mutate(across(matches('ViS'), 
                  likert)) |> 
    pivot_longer(everything(), 
                 names_to = 'ViS', 
                 values_to = 'response') |> 
    count(ViS, response) |> 
    filter(!is.na(response)) |> 
    group_by(ViS) |> 
    mutate(share = n/sum(n), 
           H_term = -share * log2(share)) |> 
    summarize(H = sum(H_term)) |> 
    arrange(desc(H))
