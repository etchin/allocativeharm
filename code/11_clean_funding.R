## Purpose: Clean funding and join with data on census tracts that gained or lost funding based on alternative models
#
#

library(readr)
library(dplyr)
library(grf)
library(ggplot2)
source(here::here("code","00_utils.R"))

deltaTracts <- read_csv(here::here("data","processed","deltaTracts.csv"))
oldCesDF <- read_csv(here::here("data", basename(url_ces3)))
tractsCes2 <- read_csv(here::here("data","processed","ces3.0_funding_amounts.csv"))
tractsCes2_removed <- read_csv(here::here("data","processed","ces3.0_funding_amounts_removed.csv"))
tractsCes2_v3 <- read_csv(here::here("data","processed","ces3.0_funding_amounts_v3.csv"))

grfDF <- read_csv(here::here("data","processed","grfDF.csv"))
race_poverty_est_wide <-  read_csv(here::here("data","acs_race_poverty_estimates.csv"))
oldCesDF2 <- oldCesDF %>% select(-starts_with(c("Nearby","Longitude","Latitude","CES")))

generateFundingDF <- function(tractsCesDF) {
  fundingDF <- tractsCesDF %>% 
    filter(!is.na(`Census Tract`)) %>%
    left_join(deltaTracts,by=c("Census Tract"="Census.Tract")) %>% 
    mutate(gainedFundingFromChanges = ifelse(is.na(dac),0,1-dac),
           lostFundingFromChanges = ifelse(is.na(dac),0,dac)) %>% 
    select(-dac) %>% 
    left_join(oldCesDF2) %>%
    left_join(grfDF) %>%
    mutate(rGain = ifelse((rDac == 1 & origDac == 0),1,0),
           dGain = ifelse((dDac == 1 & origDac == 0),1,0)) %>% 
    left_join(race_poverty_est_wide,by="Census Tract") %>% #poverty.x is the correct one
    rowwise() %>% 
    mutate(totalSum = sum(c(funding.dac,funding.buffer,funding.li,funding.other))) %>% 
    ungroup() %>% 
    mutate(`Census Tract` = as.character(`Census Tract`),
           ZIP = as.character(ZIP),
           `SB 535 Disadvantaged Community` = factor(`SB 535 Disadvantaged Community`),
           logTotalSum = log(totalSum))

  fundingDF <- fundingDF %>%
    select(-c(Poverty.x,TotalWhite,PovertyWhite,
              TotalBlack,PovertyBlack,TotalAIAN,
              TotalAsian,PovertyAsian,TotalNHPI,
              PovertyNHPI,TotalOther,PovertyOther,
              TotalMixed,TotalWhiteNH,PovertyWhiteNH,
              TotalLatine,PovertyLatine,origDac,rDac,dDac)) #remove total pop values, keep percentages
  fundingDF <- fundingDF %>% filter(!is.na(`CES 3.0 Percentile`)) %>% distinct()
  return(fundingDF)
}

fundingDF <- generateFundingDF(tractsCes2)
fundingDF_removed <- generateFundingDF(tractsCes2_removed)
fundingDF_v3 <- generateFundingDF(tractsCes2_v3)
write_csv(fundingDF, here::here("data","processed","dacFunding.csv"))
write_csv(fundingDF_removed, here::here("data","processed","dacFunding_ctfOnly.csv"))
write_csv(fundingDF_v3, here::here("data","processed","dacFunding_v3.csv"))

