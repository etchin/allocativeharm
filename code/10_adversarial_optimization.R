## Purpose: Adversarially optimize funding weights by political party affiliation
#
#
library(stats)
library(optimx)
library(dfoptim)
library(readr)
library(data.table)
library(dplyr)
source(here::here("code","00_utils.R"))

set.seed(888)

assemblyDistricts <- read_csv(here::here("data","processed","assemblyDistrictsParties.csv"))

cesDF <- fread(here::here("data","processed","ces.csv"))
cesDF <- cesDF %>% 
  mutate_at(.vars = all_of(allVars), .funs = list(Scaled = ~as.vector(scale(.))))

cesDF <- cesDF %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`)) %>%
  left_join(assemblyDistricts) %>% 
  filter(!is.na(`CES 4.0 Score`))
cesDF$dac <- as.numeric(cesDF$`CES 4.0 Percentile` >= 75)
acs_race_poverty_estimates <- read_csv(here::here("data","acs_race_poverty_estimates.csv"))
cesDF_race <- left_join(cesDF,acs_race_poverty_estimates,by="Census Tract")

#data is an nxm matrix, weights for weighted mean are an mx1 vector, use matrix multiplication to get weighted mean
#result is vector of scores, multiply vector of scores against desired vector of attributes, and maximize for the sum of that vector

w1 = runif(length(envExpVars),min=.1,max=.9)
w2 = runif(length(envEffVars),min=.1,max=.9)
w3 = runif(length(popVars),min=.1,max=.9)
w4 = runif(length(sesVars),min=.1,max=.9)
w5 = runif(1,min=.1,max=.9)
w6 = runif(1,min=.1,max=.9)
wV = c(w1,w2,w3,w4,w5,w6)

print("Simulating an adversarial actor by optimizing weighting by race and political party affiliation")

tol <- 1e-16
optimTest <- hjkb(wV,calculateWeightedDacScore,lower=.1,
                  upper=.9,control=c(maximize=T,tol=tol),
                  optim=T,data=cesDF,suffix="_Pctl",
                  raceDF = cesDF_race,attribOpt = "pWhiteNH")
optimTestR <- hjkb(wV,calculateWeightedDacScore,lower=.1,
                  upper=.9,control=c(maximize=T,tol=tol,info=T),
                  optim=T,data=cesDF,suffix="_Pctl",
                  raceDF = cesDF_race,attribOpt = "Republican")
optimTestD <- hjkb(wV,calculateWeightedDacScore,lower=.1,
                   upper=.9,control=c(maximize=T,tol=tol,info=T),
                   optim=T,data=cesDF,suffix="_Pctl",
                   raceDF = cesDF_race,attribOpt = "Democratic")
optimTestpoc <- hjkb(wV,calculateWeightedDacScore,lower=.1,
                   upper=.9,control=c(maximize=T,tol=tol,info=T),
                   optim=T,data=cesDF,suffix="_Pctl",
                   raceDF = cesDF_race,attribOpt = "poc")
#save params from four opt runs, make plots out of them
optParsW <- optimTest$par
optParsR <- optimTestR$par
optParsD <- optimTestD$par
optParsPOC <- optimTestpoc$par

optParsDF <- data.frame(optParsW,optParsPOC,
                        optParsR,optParsD)

write_csv(optParsDF,here::here("data","processed","optParsDF.csv"))
