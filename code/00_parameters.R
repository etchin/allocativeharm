#################################################
# Please input the following prior to running data files
census_key = ""
#################################################

if(census_key == ""){
  print("Please add your key to use the Census API before proceeding")
  print("You can request an API key at https://api.census.gov/data/key_signup.html")
  return(1)
}


# Set URLs for files
## PLACES: Local Data for Better Health, Census Tract Data 2020 release
url_cdc_places <- "https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/4ai3-zynv"
finame_cdc_places <- "PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2020_release.csv"

## CalEnviroScreen 2021 (download and unzip files into the ./data folder)
# https://oehha.ca.gov/media/downloads/calenviroscreen/document/calenviroscreen40resultsdatadictionaryf2021.zip
url_ces4 <- "https://oehha.ca.gov/media/downloads/calenviroscreen/document/calenviroscreen40resultsdatadictionaryf2021.zip"
finame_ces_data <- "calenviroscreen40resultsdatadictionary_F_2021.xlsx"
finame_ces_data_dict <- "calenviroscreen40resultsdatadictionary_F_2021.pdf"

## CalEnviroScreen, version 3.0
url_ces3 <- "https://data.ca.gov/dataset/0bd5f40b-c59b-4183-be22-d057eae8383c/resource/89b3f4e9-0bf8-4690-8c6f-715a717f3fae/download/calenviroscreen-3.0-results-june-2018-update.csv"
finame_ces3 <- "calenviroscreen-3.0-results-june-2018-update.csv"

## California GGRF
url_funding <- "https://www.arb.ca.gov/sites/default/files/classic/cc/capandtrade/auctionproceeds/cci_2022myu_detaileddata.xlsx"

## Census block information
url_blocks <- "https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2018/2018-state-legislative-bef/sldl_2018.zip"

## AB 1550 designations for census tracts
url_designation <- "https://www.arb.ca.gov/cc/capandtrade/auctionproceeds/ccidoc/ab1550censustracts_ces3_2021.xlsx"

## Housing burden
url_hb <- "https://www.huduser.gov/portal/datasets/il/il17/Section8-FY17.xlsx"

#Variable names
envExpVars <- c("Ozone","PM2.5","DieselPM","DrinkingWater","Lead","Pesticides","ToxRelease","Traffic")
envEffVars <- c('CleanupSites','GroundwaterThreats','HazWaste','ImpWaterBodies','SolidWaste')
sesVars <- c('Education','LinguisticIsolation','Poverty','Unemployment','HousingBurden')
#sensitive population variables. list of vectors. features are weighted equally within each vector, then weighted equally between vectors.
popVars <- list(resp = c("Asthma"), childmat = c("LowBirthWeight"), heart = c("CardiovascularDisease"))

allVars <- c(envExpVars, envEffVars, unname(unlist(popVars)), sesVars)

#DAC Calculations
numTracts = 7932 #does not include NAs

#climate investment fund categories
communityDevVars <- c("Urban Greening Program",
                      "Low Carbon Economy Workforce",
                      "Training and Workforce Development Program",
                      "Urban and Community Forestry Program",
                      "Affordable Housing and Sustainable Communities",
                      "Low-Income Weatherization Program",
                      "Transformative Climate Communities")

airPollutionVars <- c("Community Air Grants",
                      "Commnunity Air Grants",
                      "Community Air Protection Funds",
                      "Woodsmoke Reduction Program",
                      "Low Carbon Transportation")
transportationVars <- c("Technical Assistance",
                        "Transit and Intercity Rail Capital Program",
                        "Low Carbon Transit Operations Program"
                        )

agricultureVars <- c("Renewable Energy for Agriculture Program",
                     "Funding Agricultural Replacement Measures for Emission Reductions",
                     "Food Production Investment Program",
                     "Low Carbon Fuels Production",
                     "Climate Smart Agriculture")
adaptationInfrastructureVars <- c("Coastal Resilience Planning",
                                  "Climate Ready Program",
                                  "Forest Carbon Plan Implementation",
                                  "Climate Adaptation and Resiliency Program",
                                  "Fire Prevention Program",
                                  "Wetlands and Watershed Program",
                                  "Forest Health Program")
utilitiesVars <- c("Safe and Affordable Funding for Equity and Resilience (Drinking Water)",
                   "Water-Energy Efficiency",
                   "Waste Diversion Program")

