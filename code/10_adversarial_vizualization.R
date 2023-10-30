## Purpose: Visualize adversarially weighted models
#
#
source(here::here("code","00_utils.R"))
source(here::here("code","00_plot_fairness.R"))
library(data.table)
library(cowplot)
library(ggalluvial)
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
library(forcats)

optParsDF <- read_csv(here::here("data","processed","optParsDF.csv"))
race_poverty_est_wide <-  read_csv(here::here("data","acs_race_poverty_estimates.csv"))
cesDF <- read_csv(here::here("data","processed","ces.csv"))
assemblyDistricts <- read_csv(here::here("data","processed","assemblyDistrictsParties.csv"))

cesDF <- cesDF %>% 
  mutate_at(.vars = all_of(allVars), .funs = list(Scaled = ~as.vector(scale(.)))) %>%
  filter(!is.na(cesDF$`CES 4.0 Score`)) %>%
  mutate(oldDAC = as.numeric(`CES 4.0 Percentile` >= 75))

cesDF_race <- left_join(cesDF,race_poverty_est_wide,by="Census Tract")
cesDF_race$dac <- cesDF_race$`CES 4.0 Percentile` >= 75

scoreDF_W <- calculateWeightedDacScore(weightVec = optParsDF$optParsW,
                                          data = cesDF)
scoreDF_POC <- calculateWeightedDacScore(weightVec = optParsDF$optParsPOC,
                                      data = cesDF)
scoreDF_R <- calculateWeightedDacScore(weightVec = optParsDF$optParsR,
                                        data = cesDF)
scoreDF_D <- calculateWeightedDacScore(weightVec = optParsDF$optParsD,
                                       data = cesDF)

getFairnessDensityDF <- function(df1,old=F) {
  rpDF <- race_poverty_est_wide %>%
    mutate(`Census Tract` = as.numeric(`Census Tract`))
  if(old) {
    cesDF3 <- df1 %>%
      left_join(rpDF,by=c("Census.Tract"="Census Tract")) %>%
      left_join(cesDF %>% select(`Census Tract`,oldDAC),
                by=c("Census.Tract"="Census Tract")) %>% 
      filter(dac==1 | oldDAC==1)
  }
  else {
    cesDF3 <- df1 %>%
    left_join(rpDF,by=c("Census.Tract"="Census Tract")) %>% 
    filter(dac==1)
  return(cesDF3)
  }
}

plotDF_W <- getFairnessDensityDF(scoreDF_W) #160 NAs in scoreDF_W$percentile
plotDF_POC <- getFairnessDensityDF(scoreDF_POC)

dacDF <- subset(cesDF_race, dac==1)
wDacDF <- subset(cesDF_race,`Census Tract` %in% as.numeric(plotDF_W$Census.Tract))
pocDacDF <- subset(cesDF_race,`Census Tract` %in% as.numeric(plotDF_POC$Census.Tract))

p1 <- plotFairnessDensity(dacDF$pWhiteNH,wDacDF$pWhiteNH,"White",delta=F)
p2 <- plotFairnessDensity(1-dacDF$pWhiteNH,1-pocDacDF$pWhiteNH,"Racially Minoritized Population",delta=F)
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12)) +
    scale_fill_discrete(labels=c("Adversarial","Current")) +
    labs(fill="Model")
)
prow <- plot_grid(p1 + theme(legend.position="none"),
                  p2 + theme(legend.position="none")) + theme_bw()
prow
plot_grid(prow, legend, rel_widths = c(3, .4))
ggsave(here::here("figs","advOptRace.pdf"),width=10,height=3)

cesDF_race2 <- cesDF_race %>% filter(dac==1)
plotDF_R <- getFairnessDensityDF(scoreDF_R)
plotDF_D <- getFairnessDensityDF(scoreDF_D)
plotFairnessDensity(cesDF_race2$pWhiteNH,plotDF_W$pWhiteNH,"white",delta=F)

cesPol <- cesDF_race %>% 
  left_join(plotDF_R %>% select(Census.Tract, dac) %>% rename(`Census Tract` = Census.Tract, dacR = dac)) %>%
  left_join(plotDF_D %>% select(Census.Tract, dac) %>% rename(`Census Tract` = Census.Tract, dacD = dac)) %>%
  left_join(assemblyDistricts)
adDF_R <- plotDF_R %>% left_join(assemblyDistricts,by=c("Census.Tract"="Census Tract"))
adDF_D <- plotDF_D %>% left_join(assemblyDistricts,by=c("Census.Tract"="Census Tract"))

v1 <- cesPol %>% filter(dac==1) %>% select(Party) %>% 
  table() %>%  as.data.frame() %>% 
  pivot_wider(names_from = Party,values_from=Freq)
v2 <- adDF_R %>% filter(dac==1) %>% select(Party) %>% 
  table() %>%  as.data.frame() %>% 
  pivot_wider(names_from =Party,values_from=Freq)
v3 <- adDF_D %>% filter(dac==1) %>% select(Party) %>% 
  table() %>%  as.data.frame() %>% 
  pivot_wider(names_from =Party,values_from=Freq)

polDF <- rbind(v1,v2,v3) %>% 
  mutate(id=row_number()) %>% pivot_longer(cols=1:2,names_to="party")

#Advesarial examples are only for census tracts 75%+ (DAC)
polDF2 <- bind_rows(cesPol %>% mutate(model = "Original") %>% rename(score = `CES 4.0 Score`),
                    adDF_R %>% mutate(model = "Republican") %>% rename(`Census Tract` = Census.Tract),
                    adDF_D %>% mutate(model = "Democrat") %>% rename(`Census Tract` = Census.Tract)
                    ) %>%
  select(`Census Tract`, score, Party, dac, model) %>%
  distinct() %>% 
  filter(!is.na(score)) %>%
  group_by(model) %>% 
  mutate(pctl = rank(score)/n()*100) %>%
  ungroup() %>%
  mutate(pctl_range = case_when(model == "Original" ~ cut(pctl, unique(c(seq(0, 75, 25), seq(75, 100, 5))), 
                                                          labels = c("0-25%", "25-50%", "50-75%", "75-80%", "80-85%", "85-90%", "90-95%", "95-100%")),
                                TRUE ~ cut(pctl, seq(0, 100, 20), 
                                           labels = c("75-80%", "80-85%", "85-90%", "90-95%", "95-100%"))),
         model = factor(model, levels = c("Democrat","Original","Republican"))) %>%
  group_by(model, pctl_range) %>%
  mutate(pDem = sum(Party == "Democratic")/n()*100) %>% ungroup()
  
polDF2_wide <- polDF2 %>%
  pivot_wider(id_cols = c(`Census Tract`, Party), names_from = model, values_from = pctl_range, values_fn = {unique})

a1 <- ggplot(polDF2 %>% arrange(model, pctl_range, Party) %>% 
         mutate(Party = ifelse(Party == "Democratic", "Democrat", Party)),
       aes(x = model, stratum = pctl_range, alluvium = `Census Tract`, fill = Party, y = 1,
           label = pctl_range)) +
  geom_flow(aes.bind = "flows") +
  geom_stratum(color = "white", alpha = 0.5, fill = "grey80") +
  scale_x_discrete("\nModel") +
  scale_fill_manual("Political party", values = c("#01659f","#be1e2f")) +
  geom_text(stat = "stratum") +
  theme_void(base_size = 16) +
  guides(fill = guide_legend(override.aes = list(alpha=1))) +
  theme(legend.position = c(.8, .1)) + 
  scale_y_reverse()

polDF_pie <- polDF2 %>% 
  filter(pctl_range %in% c("75-80%", "80-85%", "85-90%", "90-95%", "95-100%")) %>%
  mutate(Party = ifelse(Party == "Democratic", "Democrat", Party),
         model = fct_recode(model, "Adversarial model\n(Democrat)" = "Democrat", "Adversarial model\n(Republican)" = "Republican")) %>%
  group_by(model, Party) %>% 
  summarize(value = n()) %>%
  group_by(model) %>%
  arrange(desc(Party)) %>%
  mutate(prop = value / sum(value) *100,
         csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos),
         lab = paste0(round(prop, 1), "%")
         ) %>% ungroup()

a2 <- ggplot(data=polDF_pie, aes(x=" ", y=value, group=Party, fill=Party)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(label = lab),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  coord_polar("y", start=0) + 
  scale_fill_manual("Political party", values = c("#01659f","#be1e2f")) +
  facet_grid(. ~ model) + 
  theme_void(base_size = 16) +
  theme(legend.position = "none")

g3 <- ggarrange(a2 + theme(panel.spacing = unit(3.7, "lines"), plot.margin = margin(0,1,0,1,unit="cm")), a1, nrow = 2, common.legend =  F, heights = c(1.5,4))

ggsave(here::here("figs","adversarial_model.pdf"), g3, width = 10, height = 10)

print(polDF2 %>% filter(dac == 1) %>% 
  group_by(model, Party) %>% summarise(n_party = n()) %>% 
  group_by(model) %>% mutate(n_total = sum(n_party)) %>% 
  pivot_wider(names_from = "model", values_from = "n_party") %>%
  mutate(rChange = percent((Republican - Original) / Original, accuracy = 0.1L),
         dChange = percent((Democrat - Original) / Original, accuracy = 0.1L),
         manipulability = percent(pmax(abs(Democrat - Original), abs(Original - Republican))/ n_total, accuracy = 0.1L)
         ))

plotDF_R_o <- getFairnessDensityDF(scoreDF_R,old=T)
plotDF_D_o <- getFairnessDensityDF(scoreDF_D,old=T)
adDF_R_o <- plotDF_R_o %>% left_join(assemblyDistricts,by=c("Census.Tract"="Census Tract"))
adDF_D_o <- plotDF_D_o %>% left_join(assemblyDistricts,by=c("Census.Tract"="Census Tract"))

#find the tracts that have newdac==1 when currentdac==0 under R and D
#do this by making DF with old dac, rdac, and ddac and census tracts
grfDF <- cesPol %>% select(`Census Tract`,dac) %>% 
  left_join(adDF_R_o %>% select(Census.Tract,dac),
            by=c(`Census Tract`="Census.Tract")) %>% 
  left_join(adDF_D_o %>% select(Census.Tract,dac),
            by=c(`Census Tract`="Census.Tract")) %>% 
  rename(origDac = dac.x,
         rDac = dac.y,
         dDac = dac) %>%
  mutate(origDac = as.numeric(origDac))

write_csv(grfDF,file=here::here("data","processed","grfDF.csv"))
grfDF %>% filter(origDac==0 & rDac==1) %>% dim()
grfDF %>% filter(origDac==0 & dDac==1) %>% dim()
  