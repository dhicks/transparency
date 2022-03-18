library(car)
library(tidyverse)
theme_set(theme_bw())
library(psych)
library(pander)
library(gvlma)
library(tables)
library(gghighlight)
library(yarrr)
library(lsr)

library(visdat)

library(here)

data_dir = here("data")

#read the data in
df <- read.csv(here(data_dir, 'data.csv'))


## Values & Ideology Hypotheses ----
#have to subset out the "other" and "prefer not to say" political ideology responses for the correlation
df2 <- df |> subset(political_ideology < 8)

#correlation of political ideology and preference
cor.test(df2$pref, df2$political_ideology, method = "spearman")

#do majority of conservatives prioritize public health (3,4) over economic growth (1,2)
table(df$political_ideology, df$pref)

## Consumer risk sensitivity and transparency penalty hypotheses ----

m1 <- lm(meti_mean ~ disclosure + conclusion, data = df)
m1 |> 
    summary()

#graphing with pirate plot
pirateplot(formula = meti_mean ~ disclosure + conclusion, #Formula
           data = df, #Data frame
           xlab = NULL, ylab = "Overall Trust", #Axis labels
           ylim = c(0,7), #y-axis limits
           gl.col = "gray", gl.lwd = c(.75, 0), gl.lty = 1, #gridline stuff
           pal = "black", #Color scheme
           point.o = .2, #Points
           bean.b.o = .2, #Density/Bean border transparency
           bean.f.o = .2, #Density fill transparency
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 0.5, #Point size
           cex.lab = .8, cex.names = .7) #Axis label size

## Shared Values hypothesis ----
#check how unequal the cell sizes are
#but first, for some reason, i can't do it all in one step, i need to filter Disclosure separately
df3 <- df |> filter(disclosure)
count(df3, shared_values)

#shared_values variable needs to be numeric for the correlation function
df3$shared_values <- as.numeric(df3$shared_values)
#checking if the shared_values variable correlates with participant's preferences
cor.test(df3$shared_values, df3$pref, method="spearman")

#t.test to test shared values hypothesis
test <- df3 |> with(t.test(meti_mean ~ shared_values))
test

#effect size for the t-test
cohensD(x=meti_mean~shared_values, data=df3, method="pooled")
