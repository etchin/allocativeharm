source(here::here("code","00_parameters.R"))
require(scales)
require(matrixStats)

#calculates calenviroscreen model output
calculateDacScore <- function(data, 
                              envExpVarsNew = envExpVars, envEffVarsNew = envEffVars, 
                              popVarsNew = popVars, sesVarsNew = sesVars, omitVar = NULL,
                              popWeightsNew = NULL,
                              suffix = "_Pctl", avg = NULL){
  data <- data %>% filter(!is.na(`CES 4.0 Score`))
  #average over components
  envExposure <- apply(data %>% select(paste0(envExpVarsNew, suffix)),1,mean,na.rm=T)
  envEffect <- apply(data %>% select(paste0(envEffVarsNew, suffix)),1,mean,na.rm=T)
  sesFactors <- apply(data %>% select(paste0(sesVarsNew, suffix)),1,mean,na.rm=T)
  
  #for sensitive populations, each group of population characteristics is weighted equally.
  #if popVars a vector, weights characteristics equally
  #if popVars is a list of vectors, weights characteristics within each vector equally, and weights each vector equally (average of averages)
  if(is.null(popWeightsNew)) popWeightsNew <- lapply(popVarsNew, function(x) rep(1, length(x))/length(x))
  if(typeof(popVarsNew) != "list"){
    popWeightsNew <- unname(unlist(popWeightsNew))
    sensPop <- apply(data %>% select(paste0(popVarsNew, suffix)),1,weighted.mean,w=popWeightsNew,na.rm=T)
  } else{
    sensPop <- matrix(0, nrow = nrow(data), ncol = length(popVarsNew))
    for(i in 1:length(popVarsNew)){
      tryCatch({
        sensPop[,i] <- apply(data %>% select(paste0(popVarsNew[[i]], suffix)),1,weighted.mean,w=popWeightsNew[[i]],na.rm=T)
      }, error = function(cond){
        message("ERROR: popWeights must be in the same format as popVars")
        message(paste("popVars: ", popVarsNew))
        message(paste("popWeights: ", popWeightsNew))
        message(paste("Original error message:\n", cond))
        return(NA)
      }
      )
    }
    sensPop <- apply(sensPop,1,mean,na.rm=T)
  }
  
  scoreDF <- as.data.frame(list(
    `Census Tract` = data$`Census Tract`,
    envExposure = envExposure,
    envEffect = envEffect,
    sensPop = sensPop,
    sesFactors = sesFactors
  ))
  
  #create pollution burden + pop characteristics
  scoreDF$pollutionBurden <- apply(scoreDF %>% select(envExposure, envEffect),1,weighted.mean,
                                         w=c(1,.5),na.rm=T)
  scoreDF$popChar <- rowMeans(as.matrix(scoreDF %>% select(sensPop, sesFactors)),
                              na.rm=T)
  
  if(omitVar == "envExposure" && !is.null(omitVar)) {
    scoreDF$pollutionBurden <- apply(scoreDF %>% select(envEffect),1,mean,na.rm=T)
    message("Omitting environmental exposures")
  }
  if(omitVar == "envEffect" && !is.null(omitVar)) {
    scoreDF$pollutionBurden <- apply(scoreDF %>% select(envExposure),1,mean,na.rm=T)
    message("Omitting environmental effects")
  }
  if(omitVar == "sensPop" && !is.null(omitVar)) {
    scoreDF$popChar <- rowMeans(as.matrix(scoreDF %>% select(sesFactors)),
                                na.rm=T)
    message("Omitting sensitive population characteristics")
  }
  if(omitVar == "sesFactors" && !is.null(omitVar)) {
    scoreDF$popChar <- rowMeans(as.matrix(scoreDF %>% select(sensPop)),
                                na.rm=T)
    message("Omitting socioeconomic factors")
  }
  if(!(omitVar %in% c("envExposure","envEffect","sensPop","sesFactors")) &&
     !is.null(omitVar)) {
    warning("Omitted variable not recognized, running normal calculation")
  }
  
  if(any(scoreDF$pollutionBurden_MinMax < 0, na.rm = T) | any(scoreDF$popChar < 0, na.rm = T)){
    scoreDF$pollutionBurden_MinMax <-
      rescale(scoreDF$pollutionBurden,to=c(0,10))
    scoreDF$popChar_MinMax <-
      rescale(scoreDF$popChar,to=c(0,10))
  } else {
    # Max scaling
    scoreDF$pollutionBurden_MinMax <- 10 * scoreDF$pollutionBurden / max(scoreDF$pollutionBurden, na.rm = T)
    scoreDF$popChar_MinMax <- 10 * scoreDF$popChar / max(scoreDF$popChar, na.rm = T)
  }
  if(!is.null(avg)) {
    scoreDF$score <- rowMeans(scoreDF %>% 
                                select(popChar_MinMax,
                          pollutionBurden_MinMax), na.rm=T)
    print("Averaging")
  }
  else {
    scoreDF$score <- scoreDF$popChar_MinMax*scoreDF$pollutionBurden_MinMax
  }
  scoreDF$percentile <- rank(scoreDF$score)/
    length(scoreDF$score[!is.na(scoreDF$score)])*100
  scoreDF$dac <- as.numeric(scoreDF$percentile >= 75)
  return(scoreDF)
}

#calculates calenviroscreen model output for adversarial optimization
calculateWeightedDacScore <- function(weightVec,data, 
                              envExpVarsNew = envExpVars, envEffVarsNew = envEffVars, 
                              popVarsNew = popVars, sesVarsNew = sesVars,
                              suffix = "_Pctl",attribOpt = NULL,optim=F,avg=NULL,
                              raceDF = NULL){

  w1 = weightVec[1:8]
  w2 = weightVec[9:13]
  w3 = weightVec[14:16]
  w4 = weightVec[17:21]
  w5 = weightVec[22]
  w6 = weightVec[23]
  
  if(w5>=.5) {
    suffix = "_Scaled"
  } else {
    suffix = "_Pctl"
  }
  if(w6>=.5) {
    avg = T
  } else {
    avg = NULL
  }
  #average over components
  envExposure <- rowWeightedMeans(as.matrix(data %>% 
                                              select(paste0(envExpVarsNew,
                                                            suffix))),
                                  w = w1,na.rm=T)
  envEffect <- rowWeightedMeans(as.matrix(data %>% 
                                            select(paste0(envEffVarsNew,
                                                          suffix))),
                                w = w2,na.rm=T)
  sensPop <- rowWeightedMeans(as.matrix(data %>% 
                                            select(paste0(popVarsNew,
                                                          suffix))),
                                w = w3,na.rm=T)
  sesFactors <- rowWeightedMeans(as.matrix(data %>% 
                                          select(paste0(sesVarsNew,
                                                        suffix))),
                              w = w4,na.rm=T)
  
  scoreDF <- as.data.frame(list(
    `Census Tract` = data$`Census Tract`,
    envExposure = envExposure,
    envEffect = envEffect,
    sensPop = sensPop,
    sesFactors = sesFactors
  ))
  
  #create pollution burden + pop characteristics
  scoreDF$pollutionBurden <- rowWeightedMeans(as.matrix(scoreDF %>%
                                                          select(envExposure,
                                                                 envEffect)),
                                              w=c(1,.5),na.rm=T)
  scoreDF$popChar <- rowMeans(as.matrix(scoreDF %>% select(sensPop, sesFactors)),na.rm=T)
  scoreDF$pollutionBurden_MinMax <- 10 * scoreDF$pollutionBurden / max(scoreDF$pollutionBurden, na.rm = T)
  scoreDF$popChar_MinMax <- 10 * scoreDF$popChar / max(scoreDF$popChar, na.rm = T)
  if(!is.null(avg)) {
    scoreDF$score <- rowMeans(scoreDF %>% 
                                select(popChar_MinMax,
                                       pollutionBurden_MinMax), na.rm=T)
  }
  else {
    scoreDF$score <- scoreDF$popChar_MinMax*scoreDF$pollutionBurden_MinMax
  }
  scoreDF$percentile <- rank(scoreDF$score)/
    length(scoreDF$score[!is.na(scoreDF$score)])*100
  scoreDF$dac <- as.numeric(scoreDF$percentile >= 75)
  if(!optim) {
    return(scoreDF)
  }
  if((attribOpt == "pWhiteNH")) {
    return(raceDF[scoreDF$dac==1,] %>% filter(!! sym(attribOpt) >= .5) %>% nrow())
  }
  if((attribOpt == "poc")) {
    return(raceDF[scoreDF$dac==1,] %>% filter(pWhiteNH < .5) %>% nrow())
  }
  if(attribOpt == "Republican") {
    return(raceDF[scoreDF$dac==1,] %>% filter(Party=="Republican") %>% nrow())
  }
  if(attribOpt == "Democratic") {
    return(raceDF[scoreDF$dac==1,] %>% filter(Party=="Democratic") %>% nrow())
  }
  
  
}

#calculates auc, faster implementation than auc()
auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

calculateTractDelta <- function(df1, df2,
                                numberTracts=numTracts,AUC=F,
                                oldDAC = F) {
  if(oldDAC) {
    df1 <- df1 %>% left_join(cesDF %>% select(`Census Tract`,oldDAC),
                             by=c("Census.Tract"="Census Tract"))
    df2 <- df2 %>% left_join(cesDF %>% select(`Census Tract`,oldDAC),
                             by=c("Census.Tract"="Census Tract"))
    df1$dac <- as.numeric(df1$dac | df1$oldDAC)
    df2$dac <- as.numeric(df2$dac | df2$oldDAC)
  }
  #takes two dataframes with different DAC designations and calculates difference
  if(AUC) {
    return(auroc(df1$dac,df2$dac))
  }
  else {
  return(sum(df1$dac!=df2$dac)/numTracts)
  }
  
}

getTractDeltas <- function(df1,df2) {
  #takes two dataframes with different DAC designations and outputs tracts that changed
  df3 <- df1 %>% filter(df1$dac!=df2$dac)
}

getAllPctls <- function(df1) {
  df1 <- df1 %>% select((starts_with(allVars) & ends_with("_Pctl")),
                        `Census Tract`)
  return(pivot_longer(df1,-22))
}

getAllScaled <- function(df1) {
  df1 <- df1 %>% select((starts_with(allVars) & ends_with("_Scaled")),
  `Census Tract`)
  return(pivot_longer(df1,-22))
}
