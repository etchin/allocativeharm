# Purpose: Compares differences between DAC formulations for scaling vs percentile
#          Compares differences between DAC formulations for additive vs multiplicative aggregation
#         
library(readxl)
library(dplyr)
library(summarytools)
library(broom)
library(nortest)
source(here::here("code","00_utils.R"))

cesDF <- fread(here::here("data","processed","ces_cdc.csv"))

#scale variables to 0 mean 1 sd
cesDF2 <- cesDF %>% 
  mutate_at(.vars = all_of(allVars), .funs = list(Scaled = ~as.vector(scale(.))))

scaledDF <- calculateDacScore(cesDF2, suffix = "_Scaled")
newDF <- calculateDacScore(cesDF2, suffix = "_Pctl")
avgDF <- calculateDacScore(cesDF2, suffix = "_Pctl",avg=T)

scaleChange <- calculateTractDelta(newDF,scaledDF)
avgChange <- calculateTractDelta(newDF,avgDF)
print(paste0("The percentage of tracts that change designation from switching to z-score standardization is ",
             round(scaleChange*100,2)))
print(paste0("The percentage of tracts that change designation from switching to averaging is ",
             round(avgChange*100,2)))

##Look at tracts that got changed and those that didn't
deltas <- getTractDeltas(newDF,scaledDF)
deltas2 <- subset(deltas,dac==0)
deltas3 <- subset(deltas,dac==1)
d2 <- subset(cesDF2,`Census Tract` %in% deltas2$Census.Tract)
d3 <- subset(cesDF2,`Census Tract` %in% deltas3$Census.Tract)

#look at the percentiles between dataframes
d22 <- getAllPctls(d2)
d33 <- getAllPctls(d3)

## calculate SD difference between percentiles for all variables
scaledVariables <- getAllScaled(cesDF2)
pctlVariables <- getAllPctls(cesDF2)
aggVariables <- scaledVariables
aggVariables$pctl <- pctlVariables$value
aggVariables2 <- aggVariables %>% group_by(name) %>% 
  summarize(pct50 = quantile(value,probs=.5,na.rm=T),
            pct60 = quantile(value,probs=.6,na.rm=T),
            pct70 = quantile(value,probs=.7,na.rm=T),
            pct80 = quantile(value,probs=.8,na.rm=T),
            pct90 = quantile(value,probs=.9,na.rm=T),
            pct99 = quantile(value,probs=.99,na.rm=T))
median(aggVariables2$pct80-aggVariables2$pct60)
median(aggVariables2$pct99-aggVariables2$pct80)
print(paste0("The median difference between the 60th and 80th percentiles is ",
             round(median(aggVariables2$pct80-aggVariables2$pct60),2),
             " standard deviations."
             ))
print(paste0("The median difference between the 80th and 99th percentiles is ",
             round(median(aggVariables2$pct99-aggVariables2$pct80),2),
             " standard deviations."
))
