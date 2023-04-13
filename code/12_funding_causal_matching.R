## Purpose: Run Matching analyses on funding data
#
#
library(readr)
library(dplyr)
library(ggplot2)
library(MatchIt)
library(mice)
library(cobalt)
library(tidyr)
library(CBPS)
library(mgcv)
source(here::here("code","00_utils.R"))

vars.race <- paste0("p",c("Latine","WhiteNH","Black","AIAN","Asian","NHPI","Other","Mixed"))
vars.popChar <- c(sesVars, unname(unlist(popVars)))
vars.popChar[vars.popChar == "Poverty"] <- "Poverty.y"
vars.popChar <- gsub(" |\\.","", vars.popChar)

vars.pollution <- c(envExpVars, envEffVars)
vars.pollution <- gsub(" |\\.","", vars.pollution)
vars.pollution <- vars.pollution[vars.pollution != "Lead"] # Lead not in CES 3.0

impute_data <- function(finame, writename, i.m = 10, i.maxit = 50, i.seed = 888, i.method = "pmm"){
  # reads in funding dataset
  # saves imputed dataset
  fundingDF <- read_csv(finame)
  
  colnames(fundingDF) <- gsub(" |\\.","", colnames(fundingDF))
  
  fundingDF <- fundingDF %>%
    mutate(dac = factor(ifelse(SB535DisadvantagedCommunity == "Yes", 1, 0))) %>% filter(!is.na(dac)) %>%
    mutate(CensusTract = as.character(CensusTract),
           ZIP = as.character(ZIP))
  
  df2impute <- fundingDF %>%
    select(CensusTract, CaliforniaCounty, !!vars.race, !!vars.popChar, !!vars.pollution) %>%
    mutate(CaliforniaCounty = factor(CaliforniaCounty))
  
  # Imputation
  # md.pattern(df2impute)
  fundingDF.imp <- mice(df2impute, m = i.m, maxit = i.maxit, .id = "CensusTract", seed = i.seed, method = i.method)
  
  imputed.data <- list(data = fundingDF.imp, treat_outcome = fundingDF %>% select(CensusTract, dac, logTotalSum))
  
  save(imputed.data, file = writename)
}

match_data <- function(finame, writename){
  load(finame)
  
  # densityplot(fundingDF.imp)
  
  form.treat <- reformulate(c(vars.popChar, vars.pollution), response = "dac")
  
  mods <- list()
  
  for(m in c("glm","gam","cbps")){
    for(i in 1:(imputed.data$data$m)){
      for(caliper in c(0.1,0.2,0.3)){
        df.imputed <- complete(imputed.data$data,i)
        df.imputed <- df.imputed %>% left_join(imputed.data$treat_outcome)
        
        mod.treat <- matchit(form.treat, data = df.imputed, method = "nearest", distance = m, caliper = caliper)
        # plot(mod.treat, type = "jitter", interactive = FALSE)
        mod.smd <- summary(mod.treat)$sum.matched[,"Std. Mean Diff."]
        # love.plot(mod.treat, thresholds = 0.2)
        
        df.matched <- get_matches(mod.treat)
        vars.out <- names(mod.smd)[abs(mod.smd) > 0.2]
        form.out <- reformulate(c("dac", vars.out[vars.out != "distance"]), response = "logTotalSum")
        if(any(vars.out == "distance")){
          mod.outcome <- glm(form.out, data = df.matched, weights = weights)
        } else{
          mod.outcome <- glm(form.out, data = df.matched)
        }
        mods[[paste0("m",m,"i",i,".c",caliper)]] <- list(treat = mod.treat, outcome = mod.outcome)
      }
    }
  }
  save(mods, file = writename)
}

m = 10
maxit = 50
method = "pmm"
seed <- 888

print("Imputing data and matching on main dataset")
impute_data(here::here("data","processed","dacFunding.csv"),here::here("data","processed","fundingDF_imputed.RData"), i.m = m, i.maxit = maxit, i.seed = seed, i.method = method)
match_data(here::here("data","processed","fundingDF_imputed.RData"), here::here("data","processed","fundingDF_matching.RData"))

print("Imputing data and matching on dataset with high speed rail removed")
impute_data(here::here("data","processed","dacFunding_ctfOnly.csv"),here::here("data","processed","fundingDF_imputed_ctfOnly.RData"), i.m = m, i.maxit = maxit, i.seed = seed, i.method = method)
match_data("data/processed/fundingDF_imputed_ctfOnly.RData", "data/processed/fundingDF_matching_ctfOnly.RData")

print("Imputing data and matching on dataset with programs specifying CES Version 3.0")
impute_data(here::here("data","processed","dacFunding_v3.csv"),here::here("data","processed","fundingDF_imputed_v3.RData"), i.m = m, i.maxit = maxit, i.seed = seed, i.method = method)
match_data(here::here("data","processed","fundingDF_imputed_v3.RData"), here::here("data","processed","fundingDF_matching_v3.RData"))


calculate_stats <- function(mods){
  #rubin's rules
  n <- length(mods)
  means <- sapply(mods, function(x) summary(x$outcome)$coefficients["dac1","Estimate"])
  m <- mean(means)
  variances <- sapply(mods, function(x) summary(x$outcome)$coefficients["dac1","Std. Error"]^2)
  
  v_within <- mean(variances)
  v_between <- sum((means-m)^2) / n
  v_total <- v_within + v_between + v_between / n
  
  sample_size = sapply(mods, function(x) sum(summary(x$treat)$nn["Matched",]))
  
  return(c(mean = m, se = sqrt(v_total), ss = mean(sample_size), ss_sd = sd(sample_size)))
}


dsets <- c("","_ctfOnly","_v3")

results_list <- list()

for(d in dsets){
  load(here::here("data","processed",paste0("fundingDF_matching", d, ".RData")))
  mod_names <- names(mods)
  methods <- c("glm","gam","cbps")
  calipers <- c(0.1,0.2,0.3)
  
  results <- expand_grid(methods, calipers) %>% mutate(dset = gsub("_","",ifelse(d == "", "main", d)))
  
  tmp <- apply(results, 1, function(x) calculate_stats(mods[grepl(paste0("m", x["methods"], "i[0-9]+\\.c", x["calipers"]), mod_names, perl = T)]))
  
  results_list[[d]] <- bind_cols(results, tmp %>% t())
}

results <- bind_rows(results_list) %>%
  mutate(lower = mean - 1.96*se, upper = mean + 1.96*se)

write_csv(results, here::here("data","processed","matching_results.csv"))

results_wide <- results %>%
  mutate(ci = paste0(round((exp(mean)-1)*100, digits = 1), " (", round((exp(lower)-1)*100, digits = 1), ", ", round((exp(upper)-1)*100, digits = 1), ")"), 
         ss = paste0(round(ss, digits = 1), " (", round(ss_sd, digits = 2), ")")) %>%
  arrange(dset, methods, calipers) %>%
  select(dset, methods, calipers, ci, ss) %>%
  t() %>%
  as.data.frame()

write_csv(results_wide, file = here::here("data","processed","matching_results_wide.csv"))
