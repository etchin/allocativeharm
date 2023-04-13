# Purpose: Calculates/compares DAC designation by foreign born population
#
#

source(here::here("code","00_utils.R"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

race_poverty_est_wide <- readr::read_csv(here::here("data","acs_race_poverty_estimates.csv"))
fb_poverty_est_wide <- readr::read_csv(here::here("data","acs_fb_poverty_estimates.csv"))

cesDF2 <- readr::read_csv(here::here("data","processed","ces_cdc.csv"))
cesDF3 <- cesDF2 %>% mutate(`Census Tract` = as.numeric(`Census Tract`)) %>%
  filter(!is.na(`CES 4.0 Score`)) %>%
  left_join(fb_poverty_est_wide %>% 
              select(`Census Tract`, starts_with("Total"), ends_with("poverty", ignore.case = FALSE)))

# Calculate CES with COPD instead of Asthma
scoreAsthma <- calculateDacScore(cesDF3, 
                                 popVarsNew = c("Asthma","LowBirthWeight","CardiovascularDisease"), 
                                 suffix = "_Pctl")
scoreCOPD <- calculateDacScore(cesDF3, 
                               popVarsNew = c("CDC_COPD","LowBirthWeight","CardiovascularDisease"), 
                               suffix = "_Pctl")

cesDF3$ces_asthma <- scoreAsthma$score
cesDF3$ces_copd <- scoreCOPD$score

cesDF3 <- cesDF3 %>% mutate(ces_copd_Pctl = 100*percent_rank(ces_copd),
                            ces_asthma_Pctl = 100*percent_rank(ces_asthma),
                            dac_copd = as.numeric(ces_copd_Pctl >= 75),
                            dac_asthma = as.numeric(ces_asthma_Pctl >= 75))

library(pROC)
auc(cesDF3$dac_asthma,cesDF3$dac_copd)

print("Plotting discordance of algorithms using COPD instead of Asthma")

g <- ggplot(cesDF3, aes(x = 100*TotalForeignBorn/TotalTotal)) +
  geom_smooth(aes(y = dac_copd - dac_asthma), color = "black", linewidth = 0.5, span = 1, se = T) +
  scale_y_continuous(bquote(Discordance~(Y[COPD] - Y[Asthma]))) +
  theme_classic(base_size = 16) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous("% Foreign born", expand = c(0,0), breaks = seq(0,80,20), limits = c(0,80))

ggsave(g, filename = here::here("figs","discordance.pdf"), width = 8, height = 6)
