## Purpose: Run Regression Discontinuiuty Design Analyses on funding data
#
#

library(readr)
library(dplyr)
library(grf)
library(ggplot2)
library(rddtools)
library(rdd)
source(here::here("code","00_utils.R"))

fundingDF <- read_csv(here::here("data","processed","dacFunding.csv"))
fundingDF_removed <- read_csv(here::here("data","processed","dacFunding_ctfOnly.csv"))
fundingDF_v3 <- read_csv(here::here("data","processed","dacFunding_v3.csv"))


getXYW <- function(fundingDFName, W ="SB 535 Disadvantaged Community", Y =  "logTotalSum", continuous = F) {
  #establish variables/nodes
  node_list <- list(
    X = c(#"CES 3.0 Percentile",
          colnames(fundingDFName)[c(9:15, 17:84)]), #covariates
    W = W, #treatment 
    Y = Y, #outcome
    Z = "CES 3.0 Percentile" #running variable
  )
  
  x.class <- sapply(fundingDFName %>% select(all_of(node_list$X)), class)
  x.nonnumeric <- names(x.class[x.class != "numeric"])
  x.numeric <- names(x.class[x.class == "numeric"])
  
  X <- cbind(fundingDFName %>% dplyr::select(all_of(node_list$X[node_list$X %in% x.numeric])),
             fundingDFName %>% dplyr::select(all_of(x.nonnumeric)) %>% model.matrix(~ 0 + ., ., na.action = "na.pass")) #covariates
  Y <- fundingDFName %>% select(all_of(node_list$Y)) %>% unlist() %>% unname() # outcome
  Z <- fundingDFName %>% select(all_of(node_list$Z)) %>% unlist() %>% unname() # outcome
  
  if(continuous){
    W <- fundingDFName %>% select(all_of(node_list$W)) %>% unlist() %>% unname() # continuous treatment
  } else{
    W <- fundingDFName %>% select(all_of(node_list$W)) %>% unlist() %>% unname() %>% as.integer() - 1 # binarized treatment
  }
  return(list(X,Y,W,Z))
}

fundingDF <- fundingDF %>% mutate(`SB 535 Disadvantaged Community` = factor(`SB 535 Disadvantaged Community`, levels = c("No","Yes")))
fundingDF_removed <- fundingDF_removed %>% mutate(`SB 535 Disadvantaged Community` = factor(`SB 535 Disadvantaged Community`, levels = c("No","Yes")))
fundingDF_v3 <- fundingDF_v3 %>% mutate(`SB 535 Disadvantaged Community` = factor(`SB 535 Disadvantaged Community`, levels = c("No","Yes")))


xywList <- getXYW(fundingDF)
xywList_removed <- getXYW(fundingDF_removed)
xywList_v3 <- getXYW(fundingDF_v3)

#funding estimates
dacAmount <- fundingDF %>% filter(`SB 535 Disadvantaged Community`=="Yes") %>% summarize(sum=sum(totalSum))
print(paste0("Tracts designated as disadvantaged by CalEnviroScreen received a total of ",
             dacAmount," dollars in funding."))

#RD analysis

bandwidth <- IKbandwidth(fundingDF$`CES 3.0 Percentile`, fundingDF$logTotalSum, 75)
bandwidth_v3 <- IKbandwidth(fundingDF_v3$`CES 3.0 Percentile`, fundingDF_v3$logTotalSum, 75)
bandwidth_removed <- IKbandwidth(fundingDF_removed$`CES 3.0 Percentile`, fundingDF_removed$logTotalSum, 75)

allCovariates = as.data.frame(fundingDF %>% select(pBlack,pWhiteNH,pAsian,pLatine,Poverty.y,
                                                    "Pop. Char. Pctl","Pollution Burden Pctl") %>% 
                                rename(popChar = "Pop. Char. Pctl",
                                       polBurd = "Pollution Burden Pctl"))
allCovarFormula <- "pBlack+pWhiteNH+pAsian+pLatine+Poverty.y+popChar+polBurd"

getRDDestimate <- function(covar,order,bw,covarFormula,rdDF=fundingDF) {
  rddLM <- rdd_data(y = logTotalSum, 
                             x = `CES 3.0 Percentile`, 
                             covar = covar,
                             cutpoint = 75,
                             data=rdDF) %>% 
    rdd_reg_lm(slope = "separate", covariates=covarFormula,covar.opt = list("include"), order = order,bw=bw) %>% 
    summary()
  treatmentEffect <- (exp(rddLM$coefficients[2,1])-1)*100
  CI <- (exp(rddLM$coefficients[2,2]*qnorm(0.975))-1)*100
  upper <- treatmentEffect + CI
  lower <- treatmentEffect - CI
  return(c(treatmentEffect,lower,upper))
}

#Varying over bandwidths, functional forms, and covariate adjustment
rdd_nocov_ll_ikbw <- getRDDestimate(covar=F,order=1,bw=bandwidth,
                                    covarFormula = F,rdDF=fundingDF)
rdd_nocov_o2_ikbw <- getRDDestimate(covar=F,order=2,bw=bandwidth,
                                    covarFormula=F,rdDF=fundingDF)
rdd_nocov_ll_bw10 <- getRDDestimate(covar=F,order=1,bw=10,
                                    covarFormula=F,rdDF=fundingDF)
rdd_nocov_o2_bw10 <- getRDDestimate(covar=F,order=2,bw=10,
                                    covarFormula=F,rdDF=fundingDF)
rdd_allcov_ll_ikbw <- getRDDestimate(allCovariates,order=1,
                                 bw=bandwidth,covarFormula=allCovarFormula,
                                 rdDF=fundingDF)
rdd_allcov_o2_ikbw <- getRDDestimate(allCovariates,order=2,
                                     bw=bandwidth,covarFormula=allCovarFormula,
                                     rdDF=fundingDF)
rdd_allcov_ll_bw10 <- getRDDestimate(allCovariates,order=1,
                                     bw=10,covarFormula=allCovarFormula,
                                     rdDF=fundingDF)
rdd_allcov_o2_bw10 <- getRDDestimate(allCovariates,order=2,
                                     bw=10,covarFormula=allCovarFormula,
                                     rdDF=fundingDF)

rddResults <- data.frame(rdd_allcov_ll_ikbw,rdd_nocov_ll_ikbw,
                            rdd_allcov_ll_bw10,rdd_nocov_ll_bw10,
                            rdd_allcov_o2_ikbw,rdd_nocov_o2_ikbw,
                            rdd_allcov_o2_bw10,rdd_nocov_o2_bw10
)
write_csv(rddResults,here::here("data","processed","rddResults.csv"))

#Same as above, but with different dataset specification: only using projects explictly noted as being guided by CES 3.0
rdd_nocov_ll_ikbw_v3 <- getRDDestimate(covar=F,order=1,bw=bandwidth_v3,
                                    covarFormula = F,rdDF=fundingDF_v3)
rdd_nocov_o2_ikbw_v3 <- getRDDestimate(covar=F,order=2,bw=bandwidth_v3,
                                    covarFormula=F,rdDF=fundingDF_v3)
rdd_nocov_ll_bw10_v3 <- getRDDestimate(covar=F,order=1,bw=10,
                                    covarFormula=F,rdDF=fundingDF_v3)
rdd_nocov_o2_bw10_v3 <- getRDDestimate(covar=F,order=2,bw=10,
                                    covarFormula=F,rdDF=fundingDF_v3)
rdd_allcov_ll_ikbw_v3 <- getRDDestimate(allCovariates,order=1,
                                     bw=bandwidth_v3,covarFormula=allCovarFormula,
                                     rdDF=fundingDF_v3)
rdd_allcov_o2_ikbw_v3 <- getRDDestimate(allCovariates,order=2,
                                     bw=bandwidth_v3,covarFormula=allCovarFormula,
                                     rdDF=fundingDF_v3)
rdd_allcov_ll_bw10_v3 <- getRDDestimate(allCovariates,order=1,
                                     bw=10,covarFormula=allCovarFormula,
                                     rdDF=fundingDF_v3)
rdd_allcov_o2_bw10_v3 <- getRDDestimate(allCovariates,order=2,
                                     bw=10,covarFormula=allCovarFormula,
                                     rdDF=fundingDF_v3)

rddResults_v3 <- data.frame(rdd_allcov_ll_ikbw_v3,rdd_nocov_ll_ikbw_v3,
                           rdd_allcov_ll_bw10_v3,rdd_nocov_ll_bw10_v3,
                           rdd_allcov_o2_ikbw_v3,rdd_nocov_o2_ikbw_v3,
                           rdd_allcov_o2_bw10_v3,rdd_nocov_o2_bw10_v3
                           )
write_csv(rddResults_v3,here::here("data","processed","rddResults_v3.csv"))

#Same as above, but on a dataset without high-speed rail projects
rdd_allcov_ll_ikbw_removed <- getRDDestimate(allCovariates,order=1,
                                             bw=bandwidth_removed,covarFormula=allCovarFormula,
                                             rdDF=fundingDF_removed)
rdd_nocov_ll_ikbw_removed <- getRDDestimate(covar=F,order=1,bw=bandwidth_removed,
                                       covarFormula = F,rdDF=fundingDF_removed)
rdd_allcov_ll_bw10_removed <- getRDDestimate(allCovariates,order=1,
                                             bw=10,covarFormula=allCovarFormula,
                                             rdDF=fundingDF_removed)
rdd_nocov_ll_bw10_removed <- getRDDestimate(covar=F,order=1,bw=10,
                                            covarFormula=F,rdDF=fundingDF_removed)
rdd_allcov_o2_ikbw_removed <- getRDDestimate(allCovariates,order=2,
                                             bw=bandwidth_removed,covarFormula=allCovarFormula,
                                             rdDF=fundingDF_removed)
rdd_nocov_o2_ikbw_removed <- getRDDestimate(covar=F,order=2,bw=bandwidth_removed,
                                       covarFormula=F,rdDF=fundingDF_removed)
rdd_allcov_o2_bw10_removed <- getRDDestimate(allCovariates,order=2,
                                             bw=10,covarFormula=allCovarFormula,
                                             rdDF=fundingDF_removed)
rdd_nocov_o2_bw10_removed <- getRDDestimate(covar=F,order=2,bw=10,
                                       covarFormula=F,rdDF=fundingDF_removed)

rddResults_removed <- data.frame(rdd_allcov_ll_ikbw_removed,rdd_nocov_ll_ikbw_removed,
                            rdd_allcov_ll_bw10_removed,rdd_nocov_ll_bw10_removed,
                            rdd_allcov_o2_ikbw_removed,rdd_nocov_o2_ikbw_removed,
                            rdd_allcov_o2_bw10_removed,rdd_nocov_o2_bw10_removed
)
write_csv(rddResults_removed,here::here("data","processed","rddResults_removed.csv"))

#Plot discontinuity using locally-averaged datapoints
rddPlotDF <- rdd_data(y = logTotalSum, 
         x = `CES 3.0 Percentile`, 
         covar = c(pBlack+pWhiteNH+pAsian+pLatine),
         cutpoint = 75,
         data=fundingDF %>% filter(!is.na(`CES 3.0 Percentile`)))

rddPlot <- rddPlotDF %>% 
  mutate(D = ifelse(x>=75,1,0)) %>% 
  ggplot(aes(x=x,y=y,col=factor(D))) +
  stat_summary_bin(fun='mean', binwidth=.1,
                   size=2, geom='point',alpha=.75) +
  geom_vline(xintercept=75,linetype="dashed") +
  theme_classic() + ylab("Log of Total Funding") +
  xlab("CES Percentile") +
  scale_color_manual(name = "",
                     values = c("#696969", "#266579"),
                     labels = c("Control", "Treatment")) +
  theme(legend.position="none")
ggsave(here::here("figs","rddPlot.pdf"),height=4,width=6)

#Conditionally Linear Model Forest implementation, varied over bandwidths and dataset specifications. See "Generalized Random Forests" by Athey et al.
getLmfRDD <- function(xywz, bw) {
  if(bw == "IK") {
    bandwidth <- IKbandwidth(xywz[[4]], xywz[[2]], 75)  
  }
  else{
    bandwidth <- bw
  }
  # Compute kernel weights for a triangular kernel.
  sample.weights <- kernelwts(xywz[[4]], 75, bandwidth, "triangular")
  subset <- sample.weights > 0
  lmf <- lm_forest(xywz[[1]][subset, ], xywz[[2]][subset], cbind(xywz[[3]], xywz[[4]])[subset, ],
                   sample.weights = sample.weights[subset], gradient.weights = c(1, 0),num.trees=10000,
                   ci.group.size = 20)
  tau.hat <- predict(lmf,estimate.variance = T,)
  ate <- mean(tau.hat$predictions[,1,])
  varCol <- tau.hat$variance.estimates[,1]
  seCol <- sqrt(varCol)/sqrt(20)
  Vw <- (1/611)*sum(seCol^2)
  Vb <- sum((tau.hat$predictions[,1,]-ate)^2)/610
  Vbm <- sum((tau.hat$predictions[,1,]-ate)^2)/610/611
  SE_pooled <- sqrt(Vw+Vb+Vbm)
  CI_expd <- (exp(SE_pooled*qnorm(0.975))-1)*100
  expAte <- (exp(ate)-1)*100
  lower <- expAte - CI_expd
  upper <- expAte + CI_expd
  return(c(expAte,lower,upper))
}
lmf_Orig_IK <- getLmfRDD(xywList,"IK")
lmf_Orig_bw10 <- getLmfRDD(xywList,10)
lmf_v3_IK <- getLmfRDD(xywList_v3,"IK")
lmf_v3_bw10 <- getLmfRDD(xywList_v3,10)
lmf_removed_IK <- getLmfRDD(xywList_removed,"IK")
lmf_removed_bw10 <- getLmfRDD(xywList_removed,10)

lmfResults <- data.frame(lmf_Orig_IK,lmf_Orig_bw10,
                         lmf_v3_IK,lmf_v3_bw10,
                         lmf_removed_IK,lmf_removed_bw10)
write_csv(lmfResults,here::here("data","processed","lmfResults.csv"))



