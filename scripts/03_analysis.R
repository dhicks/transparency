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


pirateplot(formula = meti_mean ~ shared_values, #Formula
           data = df3, #Data frame
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


## Variation in effects ----
# do consumer risk and transparency effects vary based on P's views on public health / economic growth
m2 <- lm(meti_mean ~ disclosure*conclusion*part_values, data = df)
m2 |> 
    summary()


pirateplot(formula = meti_mean ~ disclosure*conclusion*part_values, #Formula
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

# does shared values effect vary based on P's views
m3 <- lm(meti_mean ~ sci_values*part_values, data= df3)
m3 |> 
    summary()

pirateplot(formula = meti_mean ~ shared_values*part_values, #Formula
           data = df3, #Data frame
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

## ViSS-related analyses ----

viss_df <- read_csv(here('data', 'fa_six.csv')) |> 
    select(pid, starts_with('fa_'))
df4 <- df |> 
    left_join(viss_df, by = 'pid')

#check correlations
dcor <- cor(df4 |> select(meti_mean,
                          fa_scientism,
                          fa_vis,
                          fa_cynicism,
                          fa_power,
                          fa_textbook,
                          fa_vfi), use = "complete.obs") |> 
    formatC(digits=2,format="f")

vNames<- c("METI Mean",
           "Scientism",
           "Values in Science",
           "Cynicism",
           "Power",
           "Textbook Science",
           "Value Free Ideal")

rownames(dcor) <- paste0(1:length(vNames),". ", vNames)
colnames(dcor) <- 1:length(vNames)
dcor[lower.tri(dcor, diag = TRUE)] <- NA
dcor |> pander()

#does effect C (transparency penalty) vary depending on P's views?
m4 <- lm(meti_mean ~ disclosure*fa_scientism + disclosure*fa_vis + disclosure*fa_cynicism + disclosure*fa_power + disclosure*fa_textbook + disclosure*fa_vfi, data = df4)
m4 |> 
    summary()

#does effect D (shared values) vary
df5 <- df4 |> 
    filter(disclosure)
m5 <- lm(meti_mean ~ shared_values*fa_scientism + shared_values*fa_vis + shared_values*fa_cynicism + shared_values*fa_power + shared_values*fa_textbook + shared_values*fa_vfi, data = df5)
m5 |> 
    summary()

#does effect E (magnitude of C&D vary on P's prioritizing public health or economic growth) vary

#meti regressed onto disclosure and shared values interacting with P's values... and interacting with vis subscales?

lm(meti_mean ~ fa_scientism + fa_vis + fa_cynicism + fa_power + fa_textbook + fa_vfi, data = df4) |> summary()


m6 <- lm(meti_mean ~ part_values*fa_scientism + part_values*fa_vis + part_values*fa_cynicism + part_values*fa_power + part_values*fa_textbook + part_values*fa_vfi, data = df5)

lm(meti_mean ~ part_values*disclosure*shared_values*fa_scientism, data = df5)

m6 |> summary()

m_whatever <- lm(meti_mean ~ part_values*disclosure*conclusion, data = df4)
m_whatever |> summary()
