Data and code for “Values disclosures and trust in science: A
replication study”
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

This repository contains anonymized data and code for the paper “Values
disclosures and trust in science: A replication study” by Daniel J.
Hicks and Emilio Lobato.

# Data files

Data files are contained in the folder `data`.

The files `data/data.csv` and `data/data.Rds` contain anonymized
versions of the original data collected for this replication study. An
overview is included at the bottom of this readme.

The files `data/emad.csv` and `data/emad.Rds` contain versions of the
data from Elliot et al. experiment 1, downloaded from
<https://figshare.com/articles/dataset/Elliott_McCright_Allen_and_Dietz_Data_SPSS_Stata_/4695793>
on 2021-06-09. This dataset was reshaped and variables recoded to
facilitate parallel analysis with our original data using the code in
script `01_clean.R`. This dataset is made available under a Creative
Commons Attribution 4.0 International license.

# Reproducibility

To preserve participant anonymity, raw data (as downloaded from
Qualtrics and cleaned by script `01_clean.R`) is not publicly available.
Only analysis scripts `02_analysis.R` and `03_dag.R` are reproduced,
along with assembling the manuscript PDF.

Either method below will require prior installation of R, the `renv`
package, and [Quarto](https://quarto.org/).

## Using make

After cloning and downloading, in Bash or a similar shell run

    make install
    make

## Manually

In R, run

    renv::restore()

to create a local (within-project) package library, with the specific
versions used in this project. Then run the scripts `02_analysis.R` and
`03_dag.R`, in that order, to run the analyses. These scripts will write
plots and tables to the `out` folder, creating it if necessary.
Optionally, use `rmarkdown::render()` or the “Knit” button in RStudio to
generate HTML documents from these scripts, for comparison with the ones
included in the repository. (Be sure to rename the included HTML files
first.)

Finally, in the `paper` folder, from Bash or a similar shell run

    quarto render paper.Qmd --to pdf

to generate the PDF version of the manuscript.

# Data overview

``` r
library(skimr)

dataf = readRDS(file.path('data', 'data.Rds'))
skim(dataf)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | dataf |
| Number of rows                                   | 988   |
| Number of columns                                | 87    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 11    |
| factor                                           | 3     |
| logical                                          | 2     |
| numeric                                          | 71    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim_variable       | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| progress            |         0 |          1.00 |   2 |   3 |     0 |        2 |          0 |
| duration_in_seconds |         0 |          1.00 |   3 |   4 |     0 |      676 |          0 |
| finished            |         0 |          1.00 |   1 |   1 |     0 |        2 |          0 |
| ic                  |         0 |          1.00 |   1 |   1 |     0 |        1 |          0 |
| gender_identity     |         0 |          1.00 |   8 |  53 |     0 |        5 |          0 |
| gender_lived        |         2 |          1.00 |   8 |  57 |     0 |        7 |          0 |
| sci_values          |         0 |          1.00 |  13 |  15 |     0 |        2 |          0 |
| conclusion          |         0 |          1.00 |  11 |  19 |     0 |        2 |          0 |
| pid                 |         0 |          1.00 |  56 |  56 |     0 |      988 |          0 |
| pref                |       144 |          0.85 |   1 |   1 |     0 |        4 |          0 |
| part_values         |       144 |          0.85 |  13 |  15 |     0 |        2 |          0 |

**Variable type: factor**

| skim_variable   | n_missing | complete_rate | ordered | n_unique | top_counts                         |
|:----------------|----------:|--------------:|:--------|---------:|:-----------------------------------|
| race_ethnicity  |         0 |             1 | FALSE   |       18 | 5: 712, 3: 124, 2: 63, 4: 33       |
| religious_affil |         0 |             1 | FALSE   |       19 | 7: 455, 6: 209, 2: 122, 8: 93      |
| gender          |         2 |             1 | FALSE   |       15 | Wom: 498, Man: 458, Man: 5, Gen: 5 |

**Variable type: logical**

| skim_variable | n_missing | complete_rate | mean | count              |
|:--------------|----------:|--------------:|-----:|:-------------------|
| disclosure    |         0 |          1.00 | 0.67 | TRU: 660, FAL: 328 |
| shared_values |       144 |          0.85 | 0.49 | FAL: 433, TRU: 411 |

**Variable type: numeric**

| skim_variable         | n_missing | complete_rate |  mean |    sd |  p0 |   p25 |   p50 |   p75 | p100 | hist  |
|:----------------------|----------:|--------------:|------:|------:|----:|------:|------:|------:|-----:|:------|
| ViS01                 |         3 |          1.00 |  3.92 |  0.86 |   1 |  4.00 |  4.00 |  4.00 |    5 | ▁▁▂▇▃ |
| ViS02                 |         3 |          1.00 |  3.76 |  0.99 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▂▁▇▃ |
| ViS03                 |         2 |          1.00 |  2.95 |  1.13 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▂▇▃▇▂ |
| ViS04                 |         3 |          1.00 |  2.78 |  1.07 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▂▇▇▅▂ |
| ViS05                 |         5 |          0.99 |  3.77 |  0.91 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▁▃▇▃ |
| ViS06                 |         3 |          1.00 |  3.11 |  1.05 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▁▇▇▇▂ |
| ViS07                 |         0 |          1.00 |  3.09 |  0.97 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▁▆▇▇▁ |
| ViS08                 |         2 |          1.00 |  1.87 |  0.81 |   1 |  1.00 |  2.00 |  2.00 |    5 | ▅▇▁▁▁ |
| ViS09                 |         2 |          1.00 |  2.22 |  0.98 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▃▇▃▂▁ |
| ViS10                 |         2 |          1.00 |  2.96 |  1.11 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▂▇▆▇▂ |
| ViS11                 |         4 |          1.00 |  2.64 |  1.13 |   1 |  2.00 |  2.00 |  4.00 |    5 | ▃▇▃▅▁ |
| ViS12                 |         3 |          1.00 |  3.47 |  1.04 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▃▃▇▂ |
| ViS13                 |         1 |          1.00 |  1.69 |  0.78 |   1 |  1.00 |  2.00 |  2.00 |    5 | ▇▇▁▁▁ |
| ViS14                 |         2 |          1.00 |  2.36 |  0.98 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▂▇▂▂▁ |
| ViS15                 |         1 |          1.00 |  3.09 |  1.01 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▁▆▅▇▁ |
| ViS16                 |         0 |          1.00 |  3.24 |  1.07 |   1 |  2.00 |  4.00 |  4.00 |    5 | ▁▃▃▇▂ |
| ViS17                 |         1 |          1.00 |  2.30 |  0.96 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▃▇▃▂▁ |
| ViS18                 |         3 |          1.00 |  3.38 |  1.12 |   1 |  2.00 |  4.00 |  4.00 |    5 | ▁▅▃▇▂ |
| ViS19                 |         2 |          1.00 |  2.46 |  0.96 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▂▇▃▂▁ |
| ViS20                 |         2 |          1.00 |  3.37 |  1.17 |   1 |  2.00 |  4.00 |  4.00 |    5 | ▁▆▃▇▃ |
| ViS21                 |         1 |          1.00 |  2.60 |  1.02 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▂▇▃▃▁ |
| ViS22                 |         1 |          1.00 |  2.37 |  1.06 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▃▇▂▂▁ |
| ViS23                 |         2 |          1.00 |  3.37 |  1.11 |   1 |  2.00 |  4.00 |  4.00 |    5 | ▁▃▃▇▂ |
| ViS24                 |         6 |          0.99 |  2.79 |  1.08 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▂▇▆▅▁ |
| ViS25                 |         3 |          1.00 |  3.63 |  1.03 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▂▃▇▃ |
| ViS26                 |         6 |          0.99 |  3.70 |  1.02 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▂▂▇▃ |
| ViS27                 |         1 |          1.00 |  2.76 |  1.18 |   1 |  2.00 |  2.00 |  4.00 |    5 | ▂▇▃▅▂ |
| ViS28                 |         5 |          0.99 |  3.22 |  1.00 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▁▅▆▇▂ |
| ViS29                 |         2 |          1.00 |  3.60 |  0.84 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▁▆▇▂ |
| ViS30                 |         5 |          0.99 |  3.36 |  1.16 |   1 |  2.00 |  4.00 |  4.00 |    5 | ▂▅▆▇▅ |
| ViS31                 |         2 |          1.00 |  3.12 |  1.08 |   1 |  2.00 |  3.00 |  4.00 |    5 | ▁▆▅▇▂ |
| ViS32                 |         3 |          1.00 |  3.70 |  1.02 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▂▃▇▃ |
| ViS33                 |         4 |          1.00 |  2.26 |  1.10 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▅▇▂▂▁ |
| ViS34                 |         4 |          1.00 |  3.34 |  1.00 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▃▅▇▂ |
| ViS35                 |         3 |          1.00 |  2.47 |  1.08 |   1 |  2.00 |  2.00 |  3.00 |    5 | ▂▇▃▂▁ |
| ViS36                 |         5 |          0.99 |  3.61 |  1.09 |   1 |  3.00 |  4.00 |  4.00 |    5 | ▁▃▃▇▅ |
| SRS01                 |         0 |          1.00 |  0.60 |  0.49 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▆▁▁▁▇ |
| SRS02                 |         0 |          1.00 |  0.59 |  0.49 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▆▁▁▁▇ |
| SRS03                 |         0 |          1.00 |  0.79 |  0.41 |   0 |  1.00 |  1.00 |  1.00 |    1 | ▂▁▁▁▇ |
| SRS04                 |         0 |          1.00 |  0.57 |  0.50 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▆▁▁▁▇ |
| SRS05                 |         0 |          1.00 |  0.77 |  0.42 |   0 |  1.00 |  1.00 |  1.00 |    1 | ▂▁▁▁▇ |
| SRS06                 |         0 |          1.00 |  0.69 |  0.46 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▃▁▁▁▇ |
| SRS07                 |         0 |          1.00 |  0.68 |  0.47 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▃▁▁▁▇ |
| SRS08                 |         0 |          1.00 |  0.67 |  0.47 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▃▁▁▁▇ |
| SRS09                 |         0 |          1.00 |  0.71 |  0.45 |   0 |  0.00 |  1.00 |  1.00 |    1 | ▃▁▁▁▇ |
| SRS10                 |         0 |          1.00 |  0.49 |  0.50 |   0 |  0.00 |  0.00 |  1.00 |    1 | ▇▁▁▁▇ |
| SRS11                 |         0 |          1.00 |  0.46 |  0.50 |   0 |  0.00 |  0.00 |  1.00 |    1 | ▇▁▁▁▇ |
| age                   |         1 |          1.00 | 44.41 | 16.20 |  18 | 30.00 | 44.00 | 59.00 |   83 | ▇▆▆▇▂ |
| religious_serv        |         1 |          1.00 |  2.05 |  1.60 |   1 |  1.00 |  1.00 |  2.00 |    7 | ▇▁▁▁▁ |
| political_ideology    |        48 |          0.95 |  3.24 |  1.85 |   1 |  2.00 |  3.00 |  5.00 |    7 | ▇▃▂▂▃ |
| political_affiliation |         0 |          1.00 |  1.96 |  1.05 |   1 |  1.00 |  2.00 |  3.00 |    5 | ▇▃▅▁▁ |
| education             |         0 |          1.00 |  2.56 |  0.52 |   1 |  2.00 |  3.00 |  3.00 |    3 | ▁▁▆▁▇ |
| SRS_sum               |         0 |          1.00 |  0.64 |  0.24 |   0 |  0.45 |  0.64 |  0.82 |    1 | ▁▃▅▆▇ |
| METI01                |         0 |          1.00 |  5.08 |  1.47 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▁▂▃▃▇ |
| METI02                |         0 |          1.00 |  5.43 |  1.31 |   1 |  4.00 |  6.00 |  6.00 |    7 | ▁▁▃▃▇ |
| METI03                |         0 |          1.00 |  5.54 |  1.33 |   1 |  5.00 |  6.00 |  7.00 |    7 | ▁▁▂▂▇ |
| METI04                |         0 |          1.00 |  5.21 |  1.58 |   1 |  4.00 |  6.00 |  6.00 |    7 | ▁▁▂▃▇ |
| METI05                |         0 |          1.00 |  5.38 |  1.36 |   1 |  4.00 |  6.00 |  6.00 |    7 | ▁▁▃▃▇ |
| METI06                |         0 |          1.00 |  5.23 |  1.47 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▁▁▃▃▇ |
| METI07                |         0 |          1.00 |  4.91 |  1.59 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▂▂▃▃▇ |
| METI08                |         0 |          1.00 |  4.81 |  1.65 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▂▂▅▃▇ |
| METI09                |         0 |          1.00 |  4.69 |  1.48 |   1 |  4.00 |  4.00 |  6.00 |    7 | ▂▂▇▅▇ |
| METI10                |         0 |          1.00 |  4.70 |  1.51 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▂▂▇▅▇ |
| METI11                |         0 |          1.00 |  4.66 |  1.61 |   1 |  4.00 |  4.00 |  6.00 |    7 | ▂▂▇▃▇ |
| METI12                |         0 |          1.00 |  4.71 |  1.69 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▂▂▅▃▇ |
| METI13                |         0 |          1.00 |  4.76 |  1.76 |   1 |  4.00 |  5.00 |  6.00 |    7 | ▃▂▅▃▇ |
| METI14                |         0 |          1.00 |  4.62 |  1.55 |   1 |  4.00 |  4.00 |  6.00 |    7 | ▂▂▇▃▇ |
| meti_mean             |         0 |          1.00 |  4.98 |  1.29 |   1 |  4.07 |  5.07 |  6.00 |    7 | ▁▂▆▇▇ |
| meti_competence       |         0 |          1.00 |  5.31 |  1.25 |   1 |  4.50 |  5.50 |  6.33 |    7 | ▁▁▃▆▇ |
| meti_integrity        |         0 |          1.00 |  4.78 |  1.42 |   1 |  4.00 |  4.75 |  6.00 |    7 | ▁▃▇▇▇ |
| meti_benevolence      |         0 |          1.00 |  4.69 |  1.53 |   1 |  4.00 |  4.75 |  6.00 |    7 | ▂▃▇▇▇ |
