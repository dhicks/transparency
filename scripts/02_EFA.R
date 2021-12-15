library(car)
library(tidyverse)
library(psych)
library(pander)
library(gvlma)
library(tables)
library(here)

panderOptions("table.split.table",Inf)
panderOptions("round",2)
panderOptions("keep.trailing.zeros",TRUE)
panderOptions("table.emphasize.rownames", FALSE)
panderOptions("table.alignment.rownames", "left")
panderOptions("missing","")

#EFA for the ViS items, checking factor structure; need to check if N is large enough for a split to do CFA on
d_vis <- subset(d_clean, select = c(ViS01:ViS36))

#if N is big enough to reasonably split for EFA and CFA,
set.seed(032585) #for reproducibility
dummy_sep <- rbinom(nrow(d_vis), 1, 0.5) #create dummy indicator to randomly split sample
d_vis_efa <- d_vis[dummy_sep == 0, ] #extract data where dummy == 0
d_vis_cfa <- d_vis[dummy_sep == 1, ] #extract data where dummy == 1
vis_fa <- fa.parallel(d_vis_efa, fm = "minres", fa = "fa")

#if N is not big enough to reasonably split for EFA and CFA, then just EFA will suffice
vis_fa <- fa.parallel(d_vis, fm = "minres", fa = "fa")

#check item loadings for N factor solutions (look for substantial cross-loadings or no substantial loadings)
#do for as many factor solution options as seems prudent; what is recommended by fa.parallel, visual inspection of scree, or eigenvectors
three_factor <- fa(d_vis, nfactors = '3', rotate ="varimax")
six_factor <- fa(d_vis, nfactors = '6', rotate ="varimax")
#writing factor loadings to a csv for easier inspection
write.csv(three_factor$loadings, "K:\\UCM stuff\\2021-2022 GSR w Dr Hicks\\data\\three_factor_loadings.csv")
write.csv(six_factor$loadings, "K:\\UCM stuff\\2021-2022 GSR w Dr Hicks\\data\\three_factor_loadings.csv")

#CFA if sample is big enough to do EFA on first half and CFA on second half
#lavaan package for CFA
#specify the items in each latent variable/factor
model <- ' factor_1 =~ x1 + x2 + x3
           factor_2 =~ x4 + x5 + x6
           etc. '
fit <- cfa(model, data = d_vis_cfa)
summary(fit, fit.measures = TRUE)