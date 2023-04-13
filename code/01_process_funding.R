# Purpose: Convert funding amounts from district levels to census tract levels
#
#
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)
library(tidyr)
library(treemapify)
source(here::here("code","00_parameters.R"))
source(here::here("code","00_process_funding.R"))

print("Please run ./code/01_process_data.R before running ./code/01_process_funding.R")

print("Plotting tract designations and joining with assembly district data")
ab1550censustracts_ces3_2021 <- read_excel(here::here("data",basename(url_designation)),
                                           sheet = "All Census Tracts",
                                           guess_max = 10^6,
                                           skip = 9
)
colnames(ab1550censustracts_ces3_2021) <- gsub("\\r|\\n","", colnames(ab1550censustracts_ces3_2021))

tracts.designation <- ab1550censustracts_ces3_2021 %>%
  mutate(
    `Census Tract` = as.character(`Census Tract`),
    designation = case_when(
    `Disadvantaged Communities` == "Yes" ~ "dac",
    `Low-Income Communities` == "Yes" ~ "low income",
    `Household Wholly within Buffer` == "Yes" ~ "low income household",
    `Partial Overlap with Household Buffer` == "Yes" ~ "low income household",
    TRUE ~ "none"),
    buffer = case_when(
      `Low-Income Partial Overlap with Buffer` == "Yes" ~ 1,
      `Low-Income Wholly within Buffer` == "Yes" ~ 1,
      `Household Wholly within Buffer` == "Yes" ~ 1,
      `Partial Overlap with Household Buffer` == "Yes" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(`Census Tract`, designation, buffer)

assemblyDistricts <- read_csv(here::here("data","processed","assemblyDistrictsParties.csv")) %>% 
  mutate(`Census Tract` = as.character(`Census Tract`))

oldCesDF <- read_csv(here::here("data",basename(url_ces3)))

oldCesPercentiles <- oldCesDF %>% select(`CES 3.0 Percentile`, `Census Tract`) %>%
  mutate(`Census Tract` = as.character(`Census Tract`)) %>%
  left_join(tracts.designation) %>%
  left_join(assemblyDistricts)

rm(tracts.designation, assemblyDistricts, oldCesDF)

g.designation <- ggplot(oldCesPercentiles %>%
                          mutate(Designation = factor(case_when(designation == "dac" ~ "DAC",
                                                                designation == "low income" & buffer == 1 ~ "Low income communities within buffer",
                                                                designation == "low income" & buffer == 0 ~ "Low income communities outside of buffer",
                                                                designation == "low income household" ~ "Low income household within buffer",
                                                                TRUE ~ "Not a priority population"),
                                                      levels = c("DAC","Low income communities within buffer","Low income household within buffer", "Low income communities outside of buffer","Not a priority population")
                          )), 
                        aes(x = `CES 3.0 Percentile`, fill = Designation)) + 
  geom_histogram(position = "fill", breaks = seq(0,100,5), closed = "left", color = "white", linewidth = 0.5) +
  scale_x_continuous("CES Percentile", expand = c(0,0)) +
  scale_y_continuous("Proportion of tracts", expand = c(0,0)) +
  scale_fill_manual(values = c("#ffa600","#ef5675","#7a5195","#003f5c","grey90")) +
  theme_minimal(base_size = 12) + theme(aspect.ratio=1)

ggsave(g.designation, filename = "figs/ces_designation.pdf", width = 12, height = 8)

rm(g.designation)

vars.to.keep <- c("Project IDNumber","Reporting Cycle Name","Agency Name","Program Name","Census Tract","Senate District(s)","Assembly Districts(s)",
                  "Total Program GGRFFunding","Disadvantaged Community Census Tracts",
                  "DAC3Amount","Buffer Amount","Low Income Amount","Low Income Houshold Amount")

print("Processing funding data to product tract-level estimates")
cci_2022ar_detaileddata <- read_excel(here::here("data",basename(url_funding)),
                                      sheet = "Project List",
                                      guess_max = 10^6
) 

cci17_21 <- cci_2022ar_detaileddata %>% filter(
  grepl("^(20(17|18|19|20|21))", `Reporting Cycle Name`) 
) %>% select(!!vars.to.keep)


cci17_21_removed <- cci17_21 %>% filter(`Agency Name` != "High-Speed Rail Authority") #testing without high-speed rail
cci_v3 <- cci_2022ar_detaileddata %>% filter(CESVersion==3)


print("GENERATING MAIN FUNDING DATASET")
createFundingDF(cci17_21,oldCesPercentiles,"ces3.0_funding_amounts",parallelize = 4)

print("GENERATING FUNDING DATASET WITH HIGH SPEED RAIL PROJECT REMOVED")
createFundingDF(cci17_21_removed,oldCesPercentiles,"ces3.0_funding_amounts_removed",parallelize = 4)

print("GENERATING FUNDING DATASET FOR PROJECTS SPECIFYING CES VERSION 3")
createFundingDF(cci_v3,oldCesPercentiles,"ces3.0_funding_amounts_v3",parallelize = 4)

print("Plotting funding data")
tractsCes3 <- read_csv(here::here("data","processed","ces3.0_funding_amounts.csv"))

vars.funding <- c("dac","buffer","li","other")
tractsCes3 <- tractsCes3 %>% pivot_longer(cols=paste0("funding.",vars.funding),
                                          names_to="source",
                                          values_to="funding")

pal = c("#165a72","#2187ab","#63b1cb","grey80")

xbreaks <- seq(0,100,5)
xlabels <- as.character(xbreaks)
xlabels[seq(2, length(xlabels), 2)] <- ""

ybreaks <- seq(0,1000,400)
ylabels <- as.character(ybreaks)
ylabels[2:length(ylabels)] <- paste0("$", ylabels[2:length(ylabels)], " million")
  
g <- ggplot(tractsCes3 %>%
               mutate(source = factor(case_when(
                 grepl("dac", source) ~ "DAC",
                 grepl("buffer", source) ~ "Buffer",
                 grepl("li$", source, perl = T) ~ "Low income",
                 grepl("other", source, perl = T) ~ "Other"
               ), levels = c("DAC","Buffer","Low income","Other"))),
             aes(x=`CES 3.0 Percentile`, y=funding/10^6, fill=source)) +
  stat_summary(fun=sum,geom="bar",position="stack")+
  scale_x_binned("CalEnviroScreen score percentile", breaks = xbreaks, labels = xlabels, expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(NULL, breaks = ybreaks, labels = ylabels, expand = c(0,0)) +
  theme_classic(base_size = 16) +
  scale_fill_manual(values = pal) +
  labs(fill = "Funding Source")

ggsave(g, filename = here::here("figs","funding_bar.pdf"), width = 10, height = 7)


print("Plotting treemap of funding types")

programDF <- cci17_21 %>% 
  filter(`Total Program GGRFFunding` > 0) %>% #select only positive funds
  group_by(`Program Name`) %>%
  dplyr::summarize(dacSum = sum(DAC3Amount))

programDF$Category <- ""
programDF$Category <- ifelse(programDF$`Program Name` %in% utilitiesVars,
                             "Utilities",programDF$Category)
programDF$Category <- ifelse(programDF$`Program Name` %in% adaptationInfrastructureVars,
                             "Climate Adaptation/Infrastructure",programDF$Category)
programDF$Category <- ifelse(programDF$`Program Name` %in% agricultureVars,
                             "Food/Agriculture",programDF$Category)
programDF$Category <- ifelse(programDF$`Program Name` %in% transportationVars,
                             "Transit",programDF$Category)
programDF$Category <- ifelse(programDF$`Program Name` %in% airPollutionVars,
                             "Air Quality",programDF$Category)
programDF$Category <- ifelse(programDF$`Program Name` %in% communityDevVars,
                             "Housing, Employment, and Community Development",programDF$Category)
programDF2 <- programDF %>% filter(dacSum>0) %>% 
  group_by(Category) %>% 
  dplyr::summarize(dacSum=sum(dacSum)) %>%
  ungroup() %>%
  mutate(pDac = 100 * dacSum / sum(dacSum),
         label = paste0(Category, "\n", sprintf("%.2f", round(pDac, 2)), "%"))


cbPalette <- c("#264653", "#f9c74f", "#6a994e", "#0096c7", "#f29222", "#e95e50")

# Manually add small treemap groups
print(programDF2 %>% filter(pDac < 3))

g <- ggplot(data=programDF2 %>% mutate(label = ifelse(pDac < 3, "", label)),
            aes(fill=`Category`,
                area=dacSum,
                label = label)) + 
  geom_treemap(color = "white", size = 10) +
  geom_treemap_text(colour = "white",
                    place = "topleft", 
                    padding.x = grid::unit(5, "mm"),
                    padding.y = grid::unit(5, "mm"),
                    reflow = T,
                    size = 15) +
  scale_fill_manual(values=cbPalette) +  
  theme_void() + 
  theme(legend.position = "none")

ggsave(g, filename = "figs/fundingTreePlot.pdf",height=10,width=10)
