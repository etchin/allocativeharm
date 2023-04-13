# Purpose: 1. Computes how well each variable predicts DAC categorization
#         2. Creates correlation matrix plot
#

library(readxl)
library(dplyr)
library(pROC)
library(ggplot2)
library(tibble)
library(corrplot)
library(readr)
source(here::here("code","00_parameters.R"))

cesDF <- read_csv(here::here("data","processed","ces.csv")) %>%
  mutate(`Census Tract` = as.numeric(`Census Tract`))

cesDF$dac <- (cesDF$`CES 4.0 Percentile` >= 75)

aucList <- lapply(cesDF %>% select(all_of(allVars)), function(x) as.numeric(auc(cesDF$dac,x)))
aucDF <- data.frame(as.numeric(aucList),names(aucList))
colnames(aucDF) <- c("AUC","Variable")
aucDF$AUC <- ifelse(aucDF$AUC <= .5, 1-aucDF$AUC,aucDF$AUC)
aucDF <- aucDF[order(aucDF$AUC),]

corList <- lapply(cesDF %>% select(all_of(allVars)), function(x) 
  as.numeric(cor(cesDF$`CES 4.0 Percentile`,x,use = "complete.obs"))^2)
corDF <- data.frame(as.numeric(corList),names(corList))
colnames(corDF) <- c("R2","Variable")
corDF <- corDF[order(corDF$R2),]

print("Plotting comparisons between variables")
aucPlot <- ggplot(data=aucDF,aes(x=AUC,y=reorder(Variable,-AUC))) + 
  geom_bar(stat="identity") + 
  xlim(0,1) + theme_classic() +
  #theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Variable Name")

ggsave(aucPlot, filename = here::here("figs","aucVarPlot.pdf"),height=6,width=4)

print("Plotting correlation analysis")
varDF <- cesDF %>% select(all_of(allVars))
corMatrix <- cor(varDF[,aucDF$Variable], use = "complete.obs")
pdf(here::here("figs","corPlot.pdf"),
    height=6,width=6)
corrplot(corMatrix,method="color",type="full",
         diag=T,tl.col="black")
dev.off()

rs <- as.data.frame(rowSums(corMatrix))
rs <- tibble::rownames_to_column(rs, "Variable")

corAUC <- left_join(aucDF,rs)
print("High correlation between AUC and how correlated a variable is with other variables")
cor(corAUC$AUC,corAUC$`rowSums(corMatrix)`)

