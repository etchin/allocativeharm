#Purpose: Creates plots showing differences between CDC PLACES
#         data and hospitalization data used by CalEnviroScreen by census tract.
#
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpmisc)
library(ggpubr)
library(ggdensity)
library(wesanderson)
source(here::here("code","00_parameters.R"))

cesDF <- fread(here::here("data","processed/ces.csv")) %>%
                mutate(`Census Tract` = as.numeric(`Census Tract`))

caPlaces <- fread(here::here("data","processed/caPlaces.csv")) %>%
                mutate(`Census Tract` = as.numeric(LocationName),
                       dset = "CDC PLACES Estimates")

cdc_measures <- paste0("CDC_",c("Cancer","COPD","Smoking","Asthma","CKD","CVD"))
names(cdc_measures) <- c("Cancer (excluding skin cancer) among adults aged >=18 years",
                         "Chronic obstructive pulmonary disease among adults aged >=18 years",
                         "Current smoking among adults aged >=18 years",
                         "Current asthma among adults aged >=18 years",
                         "Chronic kidney disease among adults aged >=18 years",
                         "Coronary heart disease among adults aged >=18 years")

caPlaces_subset <- caPlaces %>%
  filter(Measure %in% names(cdc_measures)) %>%
  mutate(Measure = recode(Measure, !!!cdc_measures)) %>%
    select(`Census Tract`, Measure, Data_Value) %>%
  pivot_wider(names_from = "Measure", values_from = "Data_Value") %>%
  mutate_at(.vars = vars(unname(cdc_measures)), 
             .funs = list(Scaled = ~as.vector(scale(.)),
                          Pctl = ~percent_rank(.)))

cesDF <- left_join(cesDF,caPlaces_subset,by=c("Census Tract"))

readr::write_csv(cesDF, "data/processed/ces_cdc.csv")

healthOutcomes <- rbind(
  cesDF %>% 
    select(`Census Tract`, CardiovascularDisease, CDC_CVD) %>% 
    rename(`CalEnviroScreen Estimates` = CardiovascularDisease, `CDC PLACES Estimates` = CDC_CVD) %>% 
    mutate(measure = "Cardiovascular disease"),
  cesDF %>% 
    select(`Census Tract`, Asthma, CDC_Asthma) %>% 
    rename(`CalEnviroScreen Estimates` = Asthma, `CDC PLACES Estimates` = CDC_Asthma) %>% 
    mutate(measure = "Asthma")
)
  
ho_raw_r2 <- healthOutcomes %>% 
  group_by(measure) %>% 
  summarise(r2 = cor(`CDC PLACES Estimates`,
                     `CalEnviroScreen Estimates`, 
                     use = "complete.obs")^2) %>%
  mutate(label = paste0(
    "atop(bold('", measure, "'),italic(R^2==",round(r2, digits = 2),"))")
  )
ho_perc_r2 <- healthOutcomes %>% 
  group_by(measure) %>% 
  summarise(r2 = cor(percent_rank(`CDC PLACES Estimates`),
                     percent_rank(`CalEnviroScreen Estimates`), 
                     use = "complete.obs")^2) %>%
  mutate(label = paste0(
    "atop(bold('", measure, "'),italic(R^2==",round(r2, digits = 2),"))")
  )

print("Plotting how measurements (asthma + cardiovascular health) differ between CDC Places and CES 4.0 datasets")
g_raw <- ggplot(healthOutcomes %>% left_join(ho_raw_r2, by = "measure"), 
             aes(x = `CalEnviroScreen Estimates`, y = `CDC PLACES Estimates`)) +
  geom_bin2d(bins = 20) +
  theme_classic(base_size = 10) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_continuous("Number of\ncensus tracts") + 
  facet_wrap("~label", scales = "free", 
             labeller = label_parsed)

probs = c(seq(0.1,0.9,0.2), 0.99)
probs.lab = paste0(rev(probs)*100, "%")
pal <- wes_palette("Zissou1", length(probs), type = "continuous")
g_rawD <- ggplot(healthOutcomes %>% left_join(ho_raw_r2, by = "measure"), 
                aes(x = `CalEnviroScreen Estimates`, y = `CDC PLACES Estimates`)) +
  geom_point(alpha = 0.5, size = 0.7, shape = 20, color = "grey") +
  geom_hdr(probs = probs, alpha = 0.5, aes(fill = after_stat(probs), color = after_stat(probs))) +
  scale_fill_manual("Percentage of tracts\nbounded by contour", labels = probs.lab, values = pal) +
  scale_color_manual("Percentage of tracts\nbounded by contour", labels = probs.lab, values = pal) +
  scale_x_continuous("Emergency Department Visits (per 10,000 people)\nCalEnviroScreen Estimates", expand = c(0,0)) +
  scale_y_continuous("CDC PLACES Estimates\nCrude Prevalence (% Population)", expand = c(0,0)) +
  theme_classic2(base_size = 16) + 
  facet_wrap("~label", scales = "free", 
             labeller = label_parsed) +
  theme(strip.text.x = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"))



ggsave(g_raw, filename = here::here("figs","healthOutcomes_raw.pdf"), height = 3, width = 6)
ggsave(g_rawD, filename = here::here("figs","healthOutcomes_raw_contour.pdf"), height = 7, width = 14)
