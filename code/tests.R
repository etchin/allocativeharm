#Purpose: Checking if DAC calculations, etc. are correctly implemented
#
library(tidyverse)
library(pROC)
library(data.table)
library(testthat)
source(here::here("code","00_utils.R"))

assemblyDistricts <- read_csv(here::here("data","processed","assemblyDistrictsParties.csv"))

cesDF <- fread(here::here("data","processed","ces.csv")) %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`)) %>%
  left_join(assemblyDistricts) %>% 
  filter(!is.na(`CES 4.0 Score`))
newDF <- calculateDacScore(cesDF,suffix="_Pctl")
cesDF$dac <- as.numeric(cesDF$`CES 4.0 Percentile` >= 75)

#test if score function matches original scores
#round to 8 decimal figures to account for floating point differences
test_that("CES scores are equal",{
  expect_equal(sum(round(newDF$score,8)==round(cesDF$`CES 4.0 Score`,8))
,dim(cesDF)[1])
})
test_that("Population characteristic scores are equal",{
  expect_equal(sum(round(newDF$popChar_MinMax,8)==round(cesDF$`Pop. Char. Score`,8))
               ,dim(cesDF)[1])
})
test_that("Pollution burden scores are equal",{
  expect_equal(sum(round(newDF$pollutionBurden_MinMax,8)==round(cesDF$`Pollution Burden Score`,8))
               ,dim(cesDF)[1])
})
test_that("DAC designations are equal",{
  expect_equal(sum(newDF$dac==cesDF$dac)
               ,dim(cesDF)[1])
})
