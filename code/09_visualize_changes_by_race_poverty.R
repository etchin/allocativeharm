## Purpose: Visualize impacts of funding changes on populations by race and poverty
#
#
library(ggplot2)
library(cowplot)
library(readr)
library(ggpubr)
library(ggdensity)
source(here::here("code","00_utils.R"))
source(here::here("code","00_plot_fairness.R"))

cesDF <- read_csv(here::here("data","processed","ces.csv"))
newDF_all_changes_avg <- read_csv(here::here("data","processed","allChangesDF.csv"))

race_poverty_est_wide <-  read_csv(here::here("data","acs_race_poverty_estimates.csv"))
fb_poverty_est_wide <-  read_csv(here::here("data","acs_fb_poverty_estimates.csv"))

cesDF2 <- cesDF %>% 
  mutate_at(.vars = all_of(allVars), .funs = list(Scaled = ~as.vector(scale(.))))
newDF <- calculateDacScore(cesDF2, suffix = "_Pctl")

deltas <- getTractDeltas(newDF,newDF_all_changes_avg)
deltas_d <- getTractDeltas(newDF_all_changes_avg,newDF)
deltas$newPct <- deltas_d$percentile
deltas$diffPct <- abs(deltas$percentile-deltas$newPct)

write_csv(deltas %>% select(Census.Tract,dac),here::here("data","processed","deltaTracts.csv"))

deltas2 <- subset(deltas,dac==0) #tracts that gained funding
deltas3 <- subset(deltas,dac==1) #tracts that lost funding


rpDF <- race_poverty_est_wide %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`),
         pPoverty = Poverty / Total)
fbDF <- fb_poverty_est_wide %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`))

cesDF2$`Census Tract` <- as.numeric(cesDF2$`Census Tract`)
cesDF3 <- cesDF2 %>%
  left_join(rpDF,by="Census Tract") %>% 
  mutate(dac = ifelse(`CES 4.0 Percentile` >= 75,1,0))

newDF_all_changes_avg_dac <- newDF_all_changes_avg %>% filter(dac==1)

d2 <- subset(cesDF3,`Census Tract` %in% as.numeric(deltas2$Census.Tract))
d3 <- subset(cesDF3,`Census Tract` %in% as.numeric(deltas3$Census.Tract))

vars_race <- c("Latine","WhiteNH","Asian","Black","NHPI","AIAN")
vars_race_total <- paste0("Total", vars_race)
vars_race_p <- paste0("p", vars_race)
vars_race_poverty <- paste0(vars_race_p, "_poverty")
plotDF <- rbind(d2 %>% select(`Census Tract`, Total, pPoverty, !!vars_race_total, !!vars_race_p, !!vars_race_poverty) %>% mutate(Group = "Gain designation"),
                d3 %>% select(`Census Tract`, Total, pPoverty, !!vars_race_total, !!vars_race_p, !!vars_race_poverty) %>% mutate(Group = "Lose designation")
)

plotDF_total <- plotDF %>%
  select(`Census Tract`, !!vars_race_total, Group) %>%
  pivot_longer(cols = vars(vars_race_total), names_to = "race", values_to = "rTotal") %>%
  mutate(race = gsub("^Total", "", race, perl = T))
plotDF_p <- plotDF %>%
  select(`Census Tract`, !!vars_race_p, Group) %>%
  pivot_longer(cols = vars(vars_race_p), names_to = "race", values_to = "rProp") %>%
  mutate(race = gsub("^p", "", race, perl = T))
plotDF_poverty <- plotDF %>%
  select(`Census Tract`, !!vars_race_poverty, Group) %>%
  pivot_longer(cols = vars(vars_race_poverty), names_to = "race", values_to = "rPoverty") %>%
  mutate(race = gsub("^p", "", gsub("_poverty$", "", race, perl = T), perl = T))

plotDF2 <- plotDF_total %>%
  left_join(plotDF_p) %>%
  left_join(plotDF_poverty) %>%
  mutate(race = recode(factor(race, levels = vars_race), 
                       Latine = "Latine or Hispanic",
                       WhiteNH = "White, non-Hispanic",
                       Black = "Black",
                       Asian = "Asian",
                       AIAN = "American Indian and Alaska Native",
                       NHPI = "Native Hawaiian and Pacific Islander"
  )) %>%
  left_join(plotDF %>% select(`Census Tract`, Total, pPoverty, Group))

probs = c(0.25, 0.5, 0.75, 0.9)
probs.lab = paste0(rev(probs)*100, "%")
pal = c("#4DBBD5B2","#E64B35B2")

print("Plotting losses and gains in funding by race and poverty")

#Small sample sizes within AHPH and AIAN populations make poverty estimates by census tract highly variable.
glist.rpov.pt <- list()
glist.rpov <- list()
glist.pov.pt <- list()
glist.pov <- list()

for(ra in levels(plotDF2$race)[1:4]){
  glist.rpov.pt[[ra]] <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "race", xlimits = c(0,100), ylimits = c(0,NA), plotpoints = T) 
  if(ra == "Latine or Hispanic"){
    glist.rpov[[ra]] <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "race", xlimits = c(0,100), ylimits = c(0,NA)) 
    
  } else{
    glist.rpov[[ra]] <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "race", xlimits = c(0,NA), ylimits = c(0,NA)) 
  }
  glist.pov.pt[[ra]] <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "tract", xlimits = c(0,100), ylimits = c(0,100), plotpoints = T) 
  glist.pov[[ra]] <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "tract", xlimits = c(0,100), ylimits = c(0,50)) 
}

g.leg.h <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "tract", return_legend = "h") 
g.leg.v <- plotFairnessDensity2D(plotDF2, ra, "Population (%)", "tract", return_legend = "v") 

g.rpov.pt <- ggarrange(plotlist = glist.rpov.pt, nrow = 1, ncol = 4)
g.rpov <- ggarrange(plotlist = glist.rpov, nrow = 2, ncol = 2, legend.grob = g.leg.v, legend = "right")
g.pov.pt <- ggarrange(plotlist = glist.pov.pt,  nrow = 1, ncol = 4)
g.pov <- ggarrange(plotlist = glist.pov, nrow = 2, ncol = 2, legend.grob = g.leg.v, legend = "right")

g.combined <- ggarrange(g.pov.pt, g.rpov.pt, nrow = 2, ncol = 1, labels = c("A","B"), legend.grob = g.leg.h, legend = "bottom")
ggsave(g.pov, filename = here::here("figs","race_poverty.pdf"), width = 16, height = 14)
ggsave(g.rpov, filename = here::here("figs","race_poverty_by_race.pdf"), width = 16, height = 14)
ggsave(g.combined, filename = here::here("figs","race_poverty_both.pdf"), width = 30, height = 16)
