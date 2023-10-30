## Purpose: 1. Calculate effects of adding survey data instead of just using ER data.
##          2. Calculate effects of adding COPD data over using just asthma data.
##          3. Calculate the effects of a cumulative model, and checking its stability to change.

library(tidyverse)
library(data.table)
library(rankdist)
library(ggpubr)
library(lqr)
source(here::here("code","00_utils.R"))

cesCDC <- fread(here::here("data","processed","ces_cdc.csv")) %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`)) %>%
  filter(!is.na(`CES 4.0 Score`))
cesCDC$dac <- as.numeric(cesCDC$`CES 4.0 Percentile` >= 75)
cesCDC <- cesCDC %>% 
  mutate(CDC_Asthma_Pctl = CDC_Asthma_Pctl*100,
         CDC_CVD_Pctl = CDC_CVD_Pctl*100,
         CDC_COPD_Pctl = CDC_COPD_Pctl*100)


popVars2 <- popVars
popVars2$resp <- c(popVars$resp, "CDC_Asthma")
popVars2$heart <- c(popVars$heart, "CDC_CVD")

popVars3 <- popVars
popVars3$resp <- c(popVars$resp, "CDC_COPD")

popVars4 <- popVars
popVars4$cancer <- c("CDC_Cancer")
popVars4$climate <- c("CDC_CKD")

popVars5 <- popVars2
popVars5$resp <- c(popVars2$resp, "CDC_COPD")
popVars5$cancer <- popVars4$cancer
popVars5$climate <- popVars4$climate
popWeights5 <- list(resp = c(0.5, 0.5, 1), childmat = c(1), heart = c(0.5, 0.5), cancer = c(1), climate = c(1))

newDF <- calculateDacScore(cesCDC,suffix="_Pctl")
newDF_add_survey <- calculateDacScore(cesCDC,suffix="_Pctl",
                                        popVarsNew = popVars2)
newDF_add_copd <- calculateDacScore(cesCDC,suffix="_Pctl",
                                      popVarsNew = popVars3)
newDF_add_cancer_ckd <- calculateDacScore(cesCDC,suffix="_Pctl",
                                    popVarsNew = popVars4)
newDF_add_all <- calculateDacScore(cesCDC,suffix="_Pctl",
                                    popVarsNew = popVars5,
                                    popWeightsNew = popWeights5)

print("Calculating change between the original model and models using survey-based health data from the CDC instead of ER based health data, COPD instead of Asthma, adding cancer and CKD, and a composite of all above changes.")
print(paste0(round(calculateTractDelta(newDF,newDF_add_survey),4)*100,"% change when including survey data for asthma and cardiovascular disease"))
print(paste0(round(calculateTractDelta(newDF,newDF_add_copd),4)*100,"% change when including survey data for copd"))
print(paste0(round(calculateTractDelta(newDF,newDF_add_cancer_ckd),4)*100,"% change when including survey data for cancer and chronic kidney disease"))
print(paste0(round(calculateTractDelta(newDF,newDF_add_all),4)*100,"% change when including survey data for all of the above variables"))

#Calculate for cumulative model
print("Creating a cumulative/aggregate model")
#Generate scaled DF
allVars2 <- unique(c(allVars, unname(unlist(popVars5))))
cesCDC2 <- cesCDC %>% 
  mutate_at(.vars = vars(allVars2), .funs = list(Scaled = ~as.vector(scale(.)))) %>%
  mutate_at(.vars = vars(allVars2), .funs = list(ScaledTC = ~as.vector(scale(pmin(., quantile(., 0.95, na.rm = T))))))

#Calculate % change for all changes vs original model
print("Calculating change between the original and aggregate model")
newDF_scaled_avg_popVars <- calculateDacScore(cesCDC2, popVarsNew = popVars5,
                                               popWeightsNew = popWeights5,
                                              suffix="_Scaled",avg=T)
newDF_scaled_avg_nopopVars <- calculateDacScore(cesCDC2,suffix="_Scaled",avg=T)
newDF_scaled_mult_popVars <- calculateDacScore(cesCDC2, popVarsNew = popVars5,
                                               popWeightsNew = popWeights5, suffix="_Scaled")
newDF_scaled_mult_nopopVars <- calculateDacScore(cesCDC2, suffix="_Scaled")
newDF_noscaled_avg_nopopVars <- calculateDacScore(cesCDC2,avg=T)
newDF_noscaled_avg_popVars <- calculateDacScore(cesCDC2,avg=T,popVarsNew = popVars5,
                                                popWeightsNew = popWeights5)
newDF_noscaled_mult_popVars <- calculateDacScore(cesCDC2,popVarsNew = popVars5, popWeightsNew = popWeights5)
write_csv(newDF_scaled_avg_popVars,
          here::here("data","processed","allChangesDF.csv"))


print("Plotting the differences between models")
#make plot showing tract differences
newDF$scaled_avg_popVars <- newDF_scaled_avg_popVars$percentile
newDF$scaled_avg_nopopVars <- newDF_scaled_avg_nopopVars$percentile
newDF$scaled_mult_popVars <- newDF_scaled_mult_popVars$percentile
newDF$scaled_mult_nopopVars <- newDF_scaled_mult_nopopVars$percentile
newDF$noscaled_avg_popVars <- newDF_noscaled_avg_popVars$percentile
newDF$noscaled_avg_nopopVars <- newDF_noscaled_avg_nopopVars$percentile
newDF$noscaled_mult_popVars <- newDF_noscaled_mult_popVars$percentile

plotDF <- newDF %>% select(Census.Tract,percentile,
                           scaled_avg_popVars,scaled_avg_nopopVars,
                           scaled_mult_popVars,scaled_mult_nopopVars,
                           noscaled_avg_popVars,noscaled_avg_nopopVars,
                           noscaled_mult_popVars) %>% 
  rowwise() %>% 
  mutate(lower = min(percentile,scaled_avg_popVars,
                     scaled_avg_nopopVars,scaled_mult_popVars,
                     scaled_mult_nopopVars,noscaled_avg_popVars,
                     noscaled_avg_nopopVars,noscaled_mult_popVars
                     ),
         upper = max(percentile,scaled_avg_popVars,scaled_avg_nopopVars,
                     scaled_mult_popVars,scaled_mult_nopopVars,
                     noscaled_avg_popVars,noscaled_avg_nopopVars,
                     noscaled_mult_popVars),
         median = median(c(percentile,scaled_avg_popVars,
                           scaled_avg_nopopVars,scaled_mult_popVars,
                           scaled_mult_nopopVars,noscaled_avg_popVars,
                           noscaled_avg_nopopVars,noscaled_mult_popVars
                           )),
         mean = mean(c(percentile,scaled_avg_popVars,scaled_avg_nopopVars,
                       scaled_mult_popVars,scaled_mult_nopopVars,
                       noscaled_avg_popVars,noscaled_avg_nopopVars,
                       noscaled_mult_popVars)))

plotDF <- plotDF %>% left_join(cesCDC %>% select(`Census Tract`,oldDAC),
                               by=c("Census.Tract"="Census Tract")) %>% 
  mutate(dac = as.numeric(as.numeric(percentile) >= 75))

plotDF$newDAC <- newDF_scaled_avg_popVars$dac

plotDF$decile <- ntile(plotDF$percentile,10)

write_csv(plotDF,here::here("data","processed","sensitivityPlotDF.csv"))

print("Fitting quantile regression model on ranges")  
library(qgam)
plotDF3 <- as.data.frame(plotDF)
colnames(plotDF3) <- paste0("d.", colnames(plotDF3))

# The logit transformations
trans.logistic <- function(y, y.min, y.max, eps = 10^-6){
  return(log((y-(y.min-eps))/(y.max+eps-y)))
}
  
trans.logit <- function(y.logistic, y.min, y.max, eps = 10^-6){
  return((exp(y.logistic)*(y.max+eps)+y.min-eps) / (1+exp(y.logistic)))
}

plotDF3 <- plotDF3 %>%
  mutate(d.diff = d.upper - d.lower) %>%
  mutate_at(.vars = vars(d.lower, d.upper, d.median, d.mean, d.diff), .funs = list(L = ~trans.logistic(., 0, 100)))

qu2 = c(0.025, 0.125, 0.875, 0.975)

dof <- 4
qr.range <- mqgam(d.range~s(d.percentile, k=dof), qu = qu2, data = rbind(plotDF3 %>% mutate(d.range = d.lower_L) %>% select(d.percentile, d.range),
                                                                         plotDF3 %>% mutate(d.range = d.upper_L) %>% select(d.percentile, d.range)
                                                                         ))
#check for full convergence
# qdo(qr.range, 0.025, check)
# qdo(qr.range, 0.125, check)

xnew <- seq(0, 100, by = 0.1) 

plotPred <- data.frame(x = xnew)

for(q in qu2){
  plotPred[[paste0("range", q)]] <- sapply(qdo(qr.range, q, predict, newdata = list(d.percentile = xnew)), function(x) trans.logit(x, 0, 100))
}

range2show <- c(5, 25, 50, 75, 95)

pal <- c("#083566","#E64B35B2","#5e919e","#76b5c5", "#bbdae2")

plotPred_lower <- plotPred %>% 
  select(x, matches("^range0.(0|1|2)", perl = T)) %>%
  pivot_longer(cols = -x, names_to = "name", values_to = "lower") %>%
  mutate(band = case_when(
    name == "range0.025" ~ 95,
    name == "range0.05" ~ 90,
    name == "range0.125" ~ 75,
    name == "range0.25" ~ 50
  )) %>% select(-name)
plotPred_upper <- plotPred %>% 
  select(x, matches("^range0.(7|8|9)", perl = T)) %>%
  pivot_longer(cols =-x, names_to = "name", values_to = "upper") %>%
  mutate(band = case_when(
    name == "range0.975" ~ 95,
    name == "range0.95" ~ 90,
    name == "range0.875" ~ 75,
    name == "range0.75" ~ 50
  )) %>% select(-name)
plotPred_long <- plotPred_lower %>% left_join(plotPred_upper) %>% 
  arrange(desc(band), x)

g <- ggplot(plotPred_long %>% 
              mutate(pi = sapply(band, function(x) paste0(x, "% Prediction Interval"))) %>% 
              filter(band %in% c(75, 95))) +
  geom_linerange(data = plotDF, aes(x=percentile,ymin=lower,ymax=upper), alpha=0.3, color = "grey") +
  geom_ribbon(aes(x = x, ymin = lower, ymax = upper, fill = pi, group = pi), color = NA, alpha = 0.4) +
  geom_errorbar(data=plotPred %>% filter(x %in% range2show),
                aes(x=x, ymin=range0.025, ymax=range0.975), color = pal[1], linetype = "solid", width = 1) +
  geom_text(data=plotPred %>% filter(x %in% range2show),
            aes(label = paste0(x, "th\npercentile\nΔ ", round(range0.975-range0.025)), y = range0.975, x=x), vjust = -.25, color = pal[1], size = 4.75) +
  scale_y_continuous("Alternative Model Scores", breaks = seq(0,100,10), expand = c(0,0)) +
  geom_hline(yintercept=75, linetype="longdash", color = pal[2], linewidth = .7) +
  scale_x_continuous("Original Model Scores", breaks = seq(0,100,10), expand = c(0,0)) +
  scale_fill_manual(NULL, values = pal[3:4]) +
  theme_classic(base_size = 18) +
  theme(plot.margin = margin(t = 2.5, r = 1, l = 0.25, b = 0.25, unit = "cm"),
        legend.position=c(0.88, 0.15)) +
  guides(fill = guide_legend(override.aes = list(alpha=0.8))) +
  coord_cartesian(clip = "off")

ggsave(here::here("figs","quantreg.pdf"), g, device = cairo_pdf, width = 12, height = 12)

numTracts <- nrow(plotDF)

numGainFunding <- plotDF %>% filter(upper>=75 & percentile<75) %>% nrow() #709 could gain funding
numDAC <- plotDF %>% filter(percentile>=75) %>% nrow() #1984
pctIncreaseFunding <- numGainFunding/numDAC #percent increase in how many tracts get funding
numGainFundingUnder3 <- plotDF %>% filter(upper>=75 & percentile<75 & oldDAC==0) %>% nrow() #562 could gain funding when including 3.0 model
numDACUnder3 <- plotDF %>% filter(percentile>=75 | oldDAC==1) %>% nrow() #2288
pctIncreaseFundingUnder3 <- numGainFundingUnder3/numDACUnder3 #percent increase in how many tracts get funding when including 3.0 model

numLoseFunding <- plotDF %>% filter(lower<75 & percentile>=75) %>% nrow() #565 tracts could lose funding under different models
pctChangeOriginal <- (numGainFunding+numLoseFunding)/numTracts 
print(paste0(round(pctChangeOriginal,3)*100,"% of tracts can change designation when switching to alternative models."))

numLoseFundingUnder3 <- plotDF %>% filter(lower<75 & percentile>=75 & oldDAC==0) %>% nrow() #189 tracts could lose funding under different models when including 3.0 model
numGainFundingUnder3+numLoseFundingUnder3 #how many tracts would change funding when including 3.0 model
pctChangeUnder3 <- (numGainFundingUnder3+numLoseFundingUnder3)/numTracts 
decreaseUnder3 <- (pctChangeOriginal-pctChangeUnder3)/pctChangeOriginal 
print(paste0(round(pctChangeUnder3,3)*100,"% of tracts can change designation when honoring designations from both CES 4.0 and 3.0."))
print(paste0("This represents a ",round(decreaseUnder3,3)*100,"% decrease in model sensitivity."))


numLoseFundingUnderAll <- plotDF %>% filter(lower<75 & percentile>=75 & oldDAC==0 & newDAC==0) %>% nrow() #125 tracts could lose funding under different models when including 3 models
numGainFundingUnderAll <- plotDF %>% filter(upper>=75 & percentile<75 & oldDAC==0 & newDAC==0) %>% nrow() #244 could gain funding when including 3.0 model
pctChangeUnderAll <- (numLoseFundingUnderAll+numGainFundingUnderAll)/nrow(plotDF)
decreaseUnderAll <- (pctChangeOriginal-pctChangeUnderAll)/pctChangeOriginal #71.0% decrease when including all 3 models

print(paste0(round(pctChangeUnderAll,3)*100,"% of tracts can change designation when honoring designations from CES 4.0, CES 3.0, and a third alternative model we propose."))
print(paste0("This represents a ",round(decreaseUnderAll,3)*100,"% decrease in model sensitivity compared to using only CES 4.0."))


numDACUnderAll <- plotDF %>% filter(percentile>=75 | oldDAC==1 | newDAC==1) %>% nrow() #2606
decreaseUnderAll_relative <- (pctChangeUnder3-pctChangeUnderAll)/pctChangeUnder3 #51.1% decrease when including 3rd model on top of models 1+2
print(paste0("This represents a ",round(decreaseUnderAll_relative,3)*100,"% decrease in model sensitivity compared to using both CES 4.0 and 3.0."))


print(paste0(round(calculateTractDelta(newDF,newDF_scaled_avg_popVars),
                   4)*100,"% change when using z-score standardization, mean-aggregation, and all survey variables for health metrics"))
print(paste0(round(calculateTractDelta(newDF,newDF_scaled_avg_nopopVars),
                   4)*100,"% change when using z-score standardization and mean-aggregation"))
print(paste0(round(calculateTractDelta(newDF,newDF_scaled_mult_popVars),
                   4)*100,"% change when using z-score standardization and all survey variables for health metrics"))
print(paste0(round(calculateTractDelta(newDF,newDF_scaled_mult_nopopVars),
                   4)*100,"% change when using z-score standardization"))
print(paste0(round(calculateTractDelta(newDF,newDF_noscaled_avg_nopopVars),
                   4)*100,"% change when using mean aggregation"))
print(paste0(round(calculateTractDelta(newDF,newDF_noscaled_avg_popVars),
                   4)*100,"% change when using mean aggregation and all survey variables for health metrics"))
print(paste0(round(calculateTractDelta(newDF,newDF_noscaled_mult_popVars),
                   4)*100,"% change when using all survey variables for health metrics"))


