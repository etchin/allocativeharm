## Purpose: Adjust for SES factors on census block level
#
#
library(tidyverse)
library(readxl)
library(readr)
library(censusapi)
library(tigris)
library(sf)
source(here::here("code","00_parameters.R"))

ces4 <- read_csv(here::here("data","processed","ces.csv"))

print("Processing CBG-level census variables (except housing burden)")
counties <- counties("CA") %>% pull(COUNTYFP)

census <- counties %>% 
  map_dfr(function(county){
    
    print(county)
    
    getCensus(
      name = "acs/acs5",
      vintage = 2019,
      region = "block group:*",
      regionin = paste0("state:06+county:",county),
      key = census_key,
      vars = c(
        "B01001_001E", # pop
        "B15003_001E", # pop over 25
        "B15003_017E", # hs or more
        "B15003_018E", # hs or more
        "B15003_019E", # hs or more
        "B15003_020E", # hs or more
        "B15003_021E", # hs or more
        "B15003_022E", # hs or more
        "B15003_023E", # hs or more
        "B15003_024E", # hs or more
        "B15003_025E", # hs or more
        "C16002_001E", # households
        "C16002_004E", # limited english
        "C16002_007E", # limited english
        "C16002_010E", # limited english
        "C16002_013E", # limited english
        "C17002_001E", # Population for whom poverty status is determined
        "C17002_008E", # over 200 percent
        "B23025_001E", # pop over 16
        "B23025_005E" # unemployed
      )
    )
    
  })

print("For housing burden, we don't have access to the exact same housing burden data CalEnviroScreen uses at the tract-level, CHAS, so we create a close approximation using available ACS data. Specifically, we sepraately count housing-burdened renters and housing-burdened owners.")

print("For renters, B25074 gives us a close measure of burden but for income tiers that don't match the exact definition of 80% AMI in each county. So, we identify the tier that contains the 80% AMI definition for each county and count all renters up to and including that income tier.")
print(paste0("We download income limits for each County from ", url_hb))
print("We identify the proportion of renters in this group with housing burden over 50%.")

print("For owners, B25093 is similar to B25074 but provides age-of-householder tiers instead of income tiers. B19037 provides counts of households by age-of-householder and income tier. So using an equal distribution assumption between age-of-household and income, we estimate the number of housing-burdened households in each income tier. Otherwise, we follow the same procedure as before. We only have up to 35%+ burdened for owners, so we measure housing-burden over 35% instead of over 50%.")

print('Combining the two leads to an estimate of "housing burden over 35% or 50% for all households" at the CBG level.')

county_limits <- read_excel(here::here("data",basename(url_hb))) %>% 
  filter(State == 6) %>% 
  transmute(
    COUNTYFP = str_pad(County, 3, "left", 0),
    ami80 = l80_4,
    age_tier = case_when(
      ami80 > 200000 ~ "$200,000 or more",
      ami80 > 150000 ~ "$150,000 to $199,999",
      ami80 > 125000 ~ "$125,000 to $149,999",
      ami80 > 100000 ~ "$100,000 to $124,999",
      ami80 > 75000 ~ "$75,000 to $99,999",
      ami80 > 60000 ~ "$60,000 to $74,999",
      ami80 > 50000 ~ "$50,000 to $59,999",
      ami80 > 45000 ~ "$45,000 to $49,999",
      ami80 > 40000 ~ "$40,000 to $44,999",
      ami80 > 35000 ~ "$35,000 to $39,999",
      ami80 > 30000 ~ "$30,000 to $34,999",
      ami80 > 25000 ~ "$25,000 to $29,999",
      ami80 > 20000 ~ "$20,000 to $24,999",
      ami80 > 15000 ~ "$15,000 to $19,999",
      ami80 > 10000 ~ "$10,000 to $14,999",
      TRUE ~ "Less than $10,000"
    ),
    rent_tier = case_when(
      ami80 > 100000 ~ "$100,000 or more",
      ami80 > 75000 ~ "$75,000 to $99,999",
      ami80 > 50000 ~ "$50,000 to $74,999",
      ami80 > 35000 ~ "$35,000 to $49,999",
      ami80 > 20000 ~ "$20,000 to $34,999",
      ami80 > 10000 ~ "$10,000 to $19,999",
      TRUE ~ "Less than $10,000"
    )
  )

age_levels <- c(
  "Less than $10,000","$10,000 to $14,999","$15,000 to $19,999","$20,000 to $24,999","$25,000 to $29,999","$30,000 to $34,999","$35,000 to $39,999","$40,000 to $44,999","$45,000 to $49,999","$50,000 to $59,999","$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")

rent_levels <- c(
  "Less than $10,000","$10,000 to $19,999","$20,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 or more")

print("We use 2017 5-yr data to match CES4.")

acs_vars_2017_5yr <-
  listCensusMetadata(
    name = "2017/acs/acs5",
    type = "variables"
  )

housing_burden <- counties %>% 
  map_dfr(function(county){
    
    print(county)
    
    age_income <- getCensus(
      name = "acs/acs5",
      vintage = 2017,
      key = census_key,
      region = "block group:*",
      regionin = paste0("state:06+county:",county),
      vars = "group(B19037)"
    ) %>% 
      mutate(
        cbg =
          paste0(state,county,tract,block_group)
      ) %>% 
      select(!c(GEO_ID,state,county,tract,block_group,NAME) & !ends_with(c("EA","MA","M"))) %>%
      pivot_longer(
        ends_with("E"),
        names_to = "variable",
        values_to = "estimate"
      ) %>%
      left_join(
        acs_vars_2017_5yr %>% 
          select(name, label), 
        by = c("variable" = "name")
      ) %>% 
      select(-variable) %>% 
      separate(
        label,
        into = c(NA,NA,"age","income"),
        sep = "!!"
      ) %>% 
      filter(!is.na(income)) %>% 
      mutate(
        income = factor(income, levels = age_levels),
        lowinc = ifelse(
          as.numeric(income) <= county_limits %>% filter(COUNTYFP == county) %>% pull(age_tier) %>% factor(levels = age_levels) %>% as.numeric(),
          estimate, 
          0
        )
      ) %>% 
      group_by(cbg, age) %>% 
      summarize(
        perc_lowinc = sum(lowinc)/sum(estimate)
      ) %>% 
      mutate(
        age = case_when(
          age == "Householder under 25 years" ~ "Householder 15 to 24 years",
          age == "Householder 25 to 44 years" ~ "Householder 25 to 34 years",
          age == "Householder 45 to 64 years" ~ "Householder 35 to 64 years",
          TRUE ~ "Householder 65 years and over"
        )
      )
    
    rent <- getCensus(
      name = "acs/acs5",
      vintage = 2017,
      region = "block group:*",
      regionin = paste0("state:06+county:",county),
      key = census_key,
      vars = c(
        "B25074_001E", # Renter-occupied housing units
        "B25074_009E", # Rent burden above 50%
        "B25074_018E",
        "B25074_027E",
        "B25074_036E",
        "B25074_045E",
        "B25074_054E",
        "B25074_063E"
      )
    ) %>% 
      transmute(
        cbg =
          paste0(state,county,tract,block_group),
        rent_lowinc_burdened = (
          B25074_009E +
            B25074_018E +
            B25074_027E +
            B25074_036E +
            B25074_045E +
            B25074_054E +
            B25074_063E
        ),
        rent_total = B25074_001E
      )
        
    own <- getCensus(
      name = "acs/acs5",
      vintage = 2017,
      region = "block group:*",
      regionin = paste0("state:06+county:",county),
      key = census_key,
      vars = c(
        "B25093_002E", # Owner-occupied by age
        "B25093_007E", # Owner burden above 35%
        "B25093_009E",
        "B25093_014E",
        "B25093_016E",
        "B25093_021E",
        "B25093_023E",
        "B25093_028E"
      )
    ) %>% 
      mutate(
        cbg =
          paste0(state,county,tract,block_group)
      ) %>% 
      select(!c(state,county,tract,block_group)) %>% 
      pivot_longer(
        ends_with("E"),
        names_to = "variable",
        values_to = "estimate"
      ) %>% left_join(
        acs_vars_2017_5yr %>% 
          select(name, label), 
        by = c("variable" = "name")
      ) %>% 
      select(-variable) %>% 
      separate(
        label,
        into = c(NA,NA,"age","burden"),
        sep = "!!"
      ) %>% 
      mutate(burden = ifelse(!is.na(burden),"Burdened","Total")) %>% 
      pivot_wider(
        names_from = "burden",
        values_from = "estimate"
      ) %>% 
      left_join(
        age_income
      ) %>% 
      mutate(
        lowinc_burdened = Burdened*perc_lowinc
      ) %>% 
      group_by(cbg) %>% 
      summarize(
        own_lowinc_burdened = sum(lowinc_burdened,na.rm=T),
        own_total = sum(Total,na.rm=T)
      ) %>% 
      left_join(
        rent
      ) %>% 
      mutate(
        perc_lowinc_burdened = (own_lowinc_burdened + rent_lowinc_burdened)/(own_total + rent_total)
      )
    
  })

chas <- read_csv(finame_chas)

chas_clean <- chas %>% 
  filter(st == "06") %>% 
  transmute(
    GEOID = paste0(st,cnty,tract),
    total = T8_est1,
    burdened = (
      T8_est10+
        T8_est23+
        T8_est36+
        T8_est49+
        T8_est76+
        T8_est89+
        T8_est102+
        T8_est115
    ),
    perc_burdened = burdened/total
  )

housing_burden_tract <- housing_burden %>% 
  mutate(tract = substr(cbg,1,11)) %>% 
  select(-cbg) %>% 
  group_by(tract) %>% 
  summarize_all(
    sum
  ) %>% 
  mutate(
    perc_lowinc_burdened = (own_lowinc_burdened + rent_lowinc_burdened)/(own_total + rent_total)
  )

compare <- chas_clean %>% 
  select(tract = GEOID, CHAS_perc = perc_burdened) %>% 
  left_join(
    housing_burden_tract
  ) %>% 
  filter(!is.na(CHAS_perc), !is.na(perc_lowinc_burdened))

print(paste0("We compare this to the CHAS measure and find a correlation of ", cor(compare$CHAS_perc, compare$perc_lowinc_burdened)))


# Finalizing CBG-level data

cbg_results <- census %>% 
  mutate(
    cbg =
      paste0(state,county,tract,block_group)
  ) %>% 
  left_join(housing_burden) %>% 
  transmute(
    cbg,
    pop = B01001_001E,
    educational_attainment = 1 - (
      B15003_017E +
        B15003_018E +
        B15003_019E +
        B15003_020E +
        B15003_021E +
        B15003_022E +
        B15003_023E +
        B15003_024E +
        B15003_025E
    )/B15003_001E,
    housing_burden = perc_lowinc_burdened,
    linguistic_isolation = (
      C16002_004E +
        C16002_007E +
        C16002_010E +
        C16002_013E 
    )/C16002_001E,
    poverty = 1-(C17002_008E/C17002_001E),
    unemployment = B23025_005E/B23025_001E
  ) %>% 
  mutate(
    across(
      educational_attainment:unemployment,
      ~(percent_rank(.)*100),
      # ~(./max(.,na.rm=T)*100),
      .names = "{col}_perc"
    )
  ) %>% 
  mutate(
    socioeconomic_factors = rowMeans(select(.,ends_with("perc")), na.rm=T)
  )

# Weighted mean 

# Next, to convert back to tract values, we do a weighted mean of CBG values, weighted by CBG population, then convert to percentile

tract_results <- cbg_results %>% 
  mutate(
    tract = substr(cbg, 1,11) %>% as.numeric()
  ) %>% 
  group_by(tract) %>% 
  summarize(
    socioeconomic_factors = weighted.mean(socioeconomic_factors, pop,na.rm=T)
  ) %>% 
  mutate(
    socioeconomic_factors = percent_rank(socioeconomic_factors)*100
  )

# Recalculate CES score

# Now we can follow the standard procedure to reproduce the full CES score.

final <- tract_results %>% 
  left_join(
    ces4 %>% transmute(
      tract = `Census Tract`,
      ces4 = `CES 4.0 Percentile`,
      pollution_burden_score = `Pollution Burden Score`,
      sensitive_population_score = rowMeans(select(.,`Asthma_Pctl`,`LowBirthWeight_Pctl`,`CardiovascularDisease_Pctl`),na.rm=T)
    )
  ) %>% 
  filter(!is.na(ces4)) %>% 
  mutate(
    population_score_unscaled = rowMeans(select(.,sensitive_population_score,socioeconomic_factors), na.rm=T),
    population_score_unscaled = ifelse(population_score_unscaled==0,NA,population_score_unscaled),
    population_score_scaled = population_score_unscaled/max(population_score_unscaled,na.rm=T)*10,
    ces4_new = pollution_burden_score*population_score_scaled,
    ces4_new = (percent_rank(ces4_new)*100) %>% round(2),
    dac_change = case_when(
      ces4 >= 75 & ces4_new < 75 ~ "Out",
      ces4 < 75 & ces4_new >= 75 ~ "In",
      TRUE ~ "Same"
    )
  )

write_csv(final, here::here("data","processed","final_cbg_model_ces4.csv"))

# Summary stats
print("Summary statistics:")
print(paste0("Number of tracts that change designation: ", sum(final$dac_change != "Same")))
print(paste0("Percent: ", round(sum(final$dac_change != "Same")/nrow(final)*100, digits = 1), "%"))

ggplot(
  data = final,
  aes(
    x = ces4,
    y = ces4_new
  )
) +
  geom_point()


final_cbg_model_ces4 <- final
plotDF <- read_csv(here::here("data","processed","sensitivityPlotDF.csv"))
plotDF2 <- plotDF %>% left_join(final_cbg_model_ces4 %>% select(tract,ces4_new),
                                by=c("Census.Tract"="tract"))
plotDF2 <- plotDF2 %>% 
  rowwise() %>% 
  mutate(lower = min(lower,ces4_new),
         upper = max(upper,ces4_new))
print(paste0("Number that would gain funding: ",
             plotDF2 %>% filter(upper>=75 & percentile<75) %>% nrow()
             ))
print(paste0("Number that would lose funding: ",
             plotDF2 %>% filter(lower<75 & percentile>=75) %>% nrow() 
))



