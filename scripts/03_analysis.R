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

#reverse code meti so that higher values = more trust
df$meti_reverse <- 8-df$meti_mean
#creating a variable for the congruence between scientist's values and participant's values
df$pref_binary <- if_else(df$pref==1 | df$pref==2, #if participant prefers economic growth
                          "1", #value of 0
                          if_else(df$pref==3 | df$pref==4, #else, check if participant prefers public health
                                  "2", #if so, value of 2
                                  "0")) #else (meaning they are NA for the question), value of 0
df$congruence <- if_else(df$pref_binary==2 & df$Values=="public health" | df$pref_binary==1 & df$Values=="economic growth", #if participant and scientist values public health OR if participant and scientist value economic growth
                         1, #value of 1
                         if_else(df$pref_binary==1 & df$Values=="public health" | df$pref_binary==2 & df$Values=="economic growth", #else, check if P and S values are not congruence
                                 2, #if so, value of 2
                                 0)) #if either pref_binary or Values is NA, then value of 0
#making congruence a factor, with labels
df$congruence <- factor(df$congruence,
                        levels=c(1,2),
                        labels=c("Congruent Values", "Incongruent Values"))

## Values & Ideology Hypotheses ----
#have to subset out the "other" and "prefer not to say" political ideology responses for the correlation
df2 <- df |> subset(PoliticalIdeology < 8)

#correlation of political ideology and preference
cor.test(df2$pref, df2$PoliticalIdeology, method = "spearman")

#do majority of conservatives prioritize public health (3,4) over economic growth (1,2)
table(df$PoliticalIdeology, df$pref)

## Consumer risk sensitivity and transparency penalty hypotheses ----

m1 <- lm(meti_reverse ~ Disclosure + Conclusion, data = df)
m1 |> 
    summary()

#graphing with pirate plot
pirateplot(formula = meti_reverse ~ Disclosure + Conclusion, #Formula
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
df3 <- df |> filter(Disclosure)
count(df3, congruence)

#congruence variable needs to be numeric for the correlation function
df3$congruence <- as.numeric(df3$congruence)
#checking if the congruence variable correlates with participant's preferences
cor.test(df3$congruence, df3$pref, method="spearman")

#t.test to test shared values hypothesis
test <- df3 |> with(t.test(meti_reverse ~ congruence))
test

#effect size for the t-test
cohensD(x=meti_reverse~congruence, data=df3, method="pooled")
