#Purpose: Processes a variety of data files (e.g., CDC Places dataset, CES versions 3.0 and 4.0 datasets, Assembly districts, etc)
#
#

library(tidycensus)
library(readxl)
library(data.table)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
source(here::here("code","00_parameters.R"))

print("PROCESSING CENSUS DATA")

  print("Downloading and processing census block level data from the 2020 Census")
  caBlockPop <- get_decennial(geography="block",
                              variables="P1_001N",
                              year=2020,
                              state="CA",
                              key = census_key)
  write_csv(caBlockPop,here::here("data","processed","caBlockPopulations.csv"))
  
  print("Downloading and processing census tract level data from the American Community Survey")
  
  census_year <- 2019
  census_vars <- load_variables(census_year, "acs5")
  
  race_code <- "B06004"
  race_poverty_code <- "B17020"
  fb_poverty_code <- "B06012"
  
  race_poverty_vars <- census_vars %>% 
    filter(grepl(paste0("^", race_poverty_code, ".*", "_00(1|2)$"), name)) %>%
    mutate(varName = paste0(
      ifelse(grepl("poverty", label), "Poverty", "Total"),
      case_when(
        grepl("WHITE ALONE, NOT HISPANIC OR LATINO", concept) ~ "WhiteNH",
        grepl("HISPANIC OR LATINO", concept) ~ "Latine",
        grepl("WHITE ALONE", concept) ~ "White",
        grepl("BLACK OR AFRICAN AMERICAN ALONE", concept) ~ "Black",
        grepl("AMERICAN INDIAN AND ALASKA NATIVE ALONE", concept) ~ "AIAN",
        grepl("ASIAN ALONE", concept) ~ "Asian",
        grepl("NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE", concept) ~ "NHPI",
        grepl("SOME OTHER RACE ALONE", concept) ~ "Other",
        grepl("TWO OR MORE RACES", concept) ~ "Mixed",
        TRUE ~ ""
      )
    ))
  giniDF <- census_vars %>% filter(name=="B19083_001")
  giniDF$varName <- "Inequality"
  race_poverty_vars <- rbind(race_poverty_vars,giniDF)
  
  fb_poverty_vars <- census_vars %>%
    filter(grepl(paste0("^", fb_poverty_code, ".*", "_0(01|02|17|18)$"), name)) %>%
    mutate(varName = paste0(
      ifelse(grepl("poverty", label), "Poverty", "Total"),
      ifelse(grepl("Foreign", label), "Foreign born","Total")
    ))
  
  print("...processing race x poverty")
  race_poverty_est <- get_acs(geography="tract",
                              state = "CA",
                              variables=race_poverty_vars$name,
                              year=census_year,
                              survey="acs5",
                              key = census_key)
  
  print("...processing foreign born x poverty")
  fb_poverty_est <- get_acs(geography="tract",
                            state = "CA",
                            variables=fb_poverty_vars$name,
                            year=census_year,
                            survey="acs5",
                            key = census_key)
  
  race_poverty_est_wide <- race_poverty_est %>%
    mutate(`Census Tract` = as.numeric(GEOID)) %>%
    left_join(race_poverty_vars %>% select(name, varName), by = c("variable" = "name")) %>%
    pivot_wider(id_cols = "Census Tract", names_from = "varName", values_from = "estimate") %>%
    mutate(pWhiteNH = TotalWhiteNH/Total,
           pLatine = TotalLatine/Total,
           pBlack = TotalBlack/Total,
           pAIAN = TotalAIAN/Total,
           pAsian = TotalAsian/Total,
           pNHPI = TotalNHPI/Total,
           pOther = TotalOther/Total,
           pMixed = TotalMixed/Total,
           pWhiteNH_poverty = PovertyWhiteNH/TotalWhiteNH,
           pLatine_poverty = PovertyLatine/TotalLatine,
           pBlack_poverty = PovertyBlack/TotalBlack,
           pAIAN_poverty = PovertyAIAN/TotalAIAN,
           pAsian_poverty = PovertyAsian/TotalAsian,
           pNHPI_poverty = PovertyNHPI/TotalNHPI,
           pOther_poverty = PovertyOther/TotalOther,
           pMixed_poverty = PovertyMixed/TotalMixed
    )
  
  write_csv(race_poverty_est_wide, file = here::here("data","acs_race_poverty_estimates.csv"))
  
  fb_poverty_est_wide <- fb_poverty_est %>% filter(!is.na(estimate)) %>%
    mutate(`Census Tract` = as.numeric(GEOID)) %>%
    left_join(fb_poverty_vars %>% select(name, varName), by = c("variable" = "name")) %>%
    pivot_wider(id_cols = "Census Tract", names_from = "varName", values_from = "estimate") %>%
    mutate(TotalNative = TotalTotal - `TotalForeign born`,
           TotalForeignBorn = `TotalForeign born`,
           pNative_poverty = (PovertyTotal - `PovertyForeign born`)/TotalNative,
           pForeignBorn_poverty = `PovertyForeign born`/`TotalForeign born`
    )
  write_csv(race_poverty_est_wide, file = here::here("data","acs_race_poverty_estimates.csv"))
  write_csv(fb_poverty_est_wide, file = here::here("data","acs_fb_poverty_estimates.csv"))
  
  
print("PROCESSING CDC DATA")
  PLACES <- fread(here::here("data",finame_cdc_places))
  caPlaces <- PLACES %>%
      filter(StateAbbr=="CA")
  fwrite(caPlaces,here::here("data","processed","caPlaces.csv"))

print("PROCESSING CES VERSION 4.0")
  sheetname_sub <- "SB535 tract all data (2022)"
  sheetname_old <- "CES4.0FINAL_results"
  cesDF <- read_excel(here::here("data", finame_ces_data), sheet = sheetname_old, guess_max = 10^6) %>%
    mutate(dset = "CalEnviroScreen Estimates")
  
  #Cleaning variable names
  pctl_vars <- colnames(cesDF)[grepl("Pctl",colnames(cesDF))]
  replace_period <- grepl("[A-Za-z]\\.", pctl_vars)
  pctl_vars_new <- c(gsub("\\.", "", pctl_vars[replace_period]),
                     pctl_vars[!replace_period])
  pctl_vars_new <- gsub("Pctl","_Pctl", gsub(" ", "", pctl_vars_new))
  pctl_vars <- c(pctl_vars[replace_period], pctl_vars[!replace_period])
  names(pctl_vars) <- pctl_vars_new
  og_vars <- trimws(gsub("Pctl", "", pctl_vars))
  names(og_vars) <- trimws(gsub("Pctl|_", "", names(pctl_vars)))
  renamed_vars <- c(pctl_vars, og_vars)
  cesDF <- cesDF %>% rename(all_of(renamed_vars))
  
print("PROCESSING CES VERSION 3.0")
  oldCesDF <- read_csv(here::here("data", finame_ces3))
  oldCesDF <- oldCesDF %>% select(`Census Tract`,`SB 535 Disadvantaged Community`)
  cesDF <- cesDF %>% left_join(oldCesDF) %>% 
    rename(oldDAC = `SB 535 Disadvantaged Community`) %>% 
    mutate(oldDAC = as.numeric(factor(oldDAC))-1)
  
  fwrite(cesDF,here::here("data","processed","ces.csv"))


print("PROCESSING ASSEMBLY DISTRICT FILES")
print("...data from political parties were scraped from https://www.assembly.ca.gov/assemblymembers and can be found in california_state_assembly_district_parties_03_23.csv")
california_state_assembly_district_parties <- read_csv(here::here("data","california_state_assembly_district_parties_03_23.csv"))

print("...joining with block-level data")
National_2018SLDL <- read_csv(here::here("data","National_2018SLDL.txt"))
National_2018SLDL$state <- substr(National_2018SLDL$BLOCKID,start=1,stop=2)
National_2018SLDL <- National_2018SLDL %>% filter(state=="06")
National_2018SLDL$CensusTract <- substr(National_2018SLDL$BLOCKID,start=1,stop=11)

asDF <- National_2018SLDL %>% group_by(CensusTract) %>% 
  mutate(nDistrictsInTract = length(unique(DISTRICT))) %>% 
  left_join(caBlockPop %>% select(GEOID,value),by=c("BLOCKID"="GEOID"))
asDF <- asDF %>% group_by(CensusTract,DISTRICT) %>% 
  mutate(nBlocksInDistrictTract = length(unique(BLOCKID)),
         populationInDistrictTract = sum(value,na.rm=T))
asDF <- asDF %>% group_by(CensusTract) %>% 
  mutate(trueDistrict = ifelse(populationInDistrictTract==max(populationInDistrictTract),
                               1,0))
cleanAsDF <- asDF %>% filter(trueDistrict==1)
cleanAsDF <- cleanAsDF %>% distinct(CensusTract,DISTRICT, .keep_all=T)
print("...some multi-district tracts don't have block-level population -- instead, assign them to districts where they have highest # of blocks")
cleanAsDF <- cleanAsDF %>% group_by(CensusTract) %>% 
  mutate(nDistrictsinTract2 = length(unique(DISTRICT)),
         trueDistrict2 = ifelse(nBlocksInDistrictTract==max(nBlocksInDistrictTract),
                                1,0))
cleanAsDF2 <- cleanAsDF %>% filter(trueDistrict2==1) %>% 
  mutate(nDistrictsinTract3 = length(unique(DISTRICT)))
print("...only one tract remains, tract #06013313103. by area is it more in district 11 than 14, so we assign it to district 11.")
cleanAsDF2 <- cleanAsDF2 %>% filter(BLOCKID!="060133131031014")

write_csv(cleanAsDF2,here::here("data","processed","cleanAssemblyDistrictsToTracts.csv"))

assemblyDistricts <- read_csv("data/processed/cleanAssemblyDistrictsToTracts.csv")
assemblyDistricts <- cleanAsDF2
assemblyDistricts <- assemblyDistricts %>% 
  rename(`Census Tract` = CensusTract,
         district = DISTRICT)
assemblyDistricts$`Census Tract` <- as.numeric(assemblyDistricts$`Census Tract`)
assemblyDistricts <- assemblyDistricts %>% 
  distinct(`Census Tract`,district) %>% 
  mutate(district = as.numeric(district)) %>% 
  left_join(california_state_assembly_district_parties %>% select(Party,District),
            by=c("district"="District"))

write_csv(assemblyDistricts,"data/processed/assemblyDistrictsParties.csv")

