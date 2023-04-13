#Purpose: Calculates/compares DAC scores when omitting certain variables
#
#
library(tidyverse)
library(pROC)
library(data.table)
library(caret)
source(here::here("code","00_utils.R"))
cesDF <- fread(here::here("data","processed","ces.csv")) %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`)) %>%
  filter(!is.na(`CES 4.0 Score`))
cesDF$dac <- as.numeric(cesDF$`CES 4.0 Percentile` >= 75)

newDF <- calculateDacScore(cesDF,suffix="_Pctl")

newDF_omit_envExposure <- calculateDacScore(cesDF,suffix="_Pctl",omitVar = "envExposure")
newDF_omit_envEffect <- calculateDacScore(cesDF,suffix="_Pctl",omitVar = "envEffect")
newDF_omit_sensPop <- calculateDacScore(cesDF,suffix="_Pctl",omitVar = "sensPop")
newDF_omit_sesFactors <- calculateDacScore(cesDF,suffix="_Pctl",omitVar = "sesFactors")

cm_omit_envExposure <- confusionMatrix(factor(newDF$dac),
                                       factor(newDF_omit_envExposure$dac))
cm_omit_envEffect <- confusionMatrix(factor(newDF$dac),
                                     factor(newDF_omit_envEffect$dac))
cm_omit_sensPop <- confusionMatrix(factor(newDF$dac),
                                   factor(newDF_omit_sensPop$dac))
cm_omit_sesFactors <- confusionMatrix(factor(newDF$dac),
                                      factor(newDF_omit_sesFactors$dac))

print("Calculating percent change when omitting any given index")

calculateTractDelta(newDF,newDF_omit_envExposure)
calculateTractDelta(newDF,newDF_omit_envEffect)
calculateTractDelta(newDF,newDF_omit_sensPop)
calculateTractDelta(newDF,newDF_omit_sesFactors)
