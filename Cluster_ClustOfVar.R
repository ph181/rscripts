
install.packages("ClustOfVar", dependencies = TRUE)
install.packages("cluster", dependencies = TRUE)
install.packages("remotes")
remotes::install_github("DavideMassidda/testing")
install.packages("psy")
install.packages("nFactors")

library(ClustOfVar)
library(cluster)
library(testing)
library(psych)
library(psy)
library(nFactors)

AX <- as.data.frame(subset(FW, select=c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13,AN1, AN2, AN3, AN4, AN5, AN6, AN7, AN8, AN9, AN10, AN11, AN12, AN13)))
AX <- na.omit(AX)
AV <- as.data.frame(subset(AX, select=c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13)))


AV <- na.omit(AV)

AV <- as.matrix(AV)




describe(AN)
describe(BN)



#If the quantitative and qualitative data are in a same dataframe, the function PCAmixdata::splitmix 
#can be used to extract automatically the qualitative and the quantitative data in two separated dataframes. 
#xquant <- AN[,c(1,2,5,7,8)] # Numeric variables
#xqual <- AN[,c(3,4,6)]      # Categorical variables
#tree <- hclustvar(xquant, xqual)

tree <- hclustvar(AN,)
plot(tree)
stab <- stability(tree, B=50) # "B=50" refers to the number of bootstrap samples to use in the estimation.
#Look at the peaks in the plot above. The maximum for stability is 1.0, so the higer the peak, the better. 
k.means <- kmeansvar(AN, init=3)
summary(k.means)#
k.means$cluster # This produces a list of what variables are in which cluster.

write.csv(k.means$cluster, file="variableClusters.csv")

###############
tree <- hclustvar(BN,)
plot(tree)
stab <- stability(tree, B=50) # "B=50" refers to the number of bootstrap samples to use in the estimation.
#Look at the peaks in the plot above. The maximum for stability is 1.0, so the higer the peak, the better. 
k.means <- kmeansvar(BN, init=3)
summary(k.means)#
k.means$cluster # This produces a list of what variables are in which cluster.
write.csv(k.means$cluster, file="variableClusters.csv")




cortest.bartlett(AN)
cortest.bartlett(BN)

KMO(AN)
KMO(BN)

BARTLETT(AN)
BARTLETT(BN)

nfactors(AN, rotate = "varimax", fm = "mle")
nfactors(BN, rotate = "varimax", fm = "mle")



ev <- eigen(cor(AN))
ap <- parallel(subject = nrow(AN), var = ncol(AN), cent = .05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

ev <- eigen(cor(BN))
ap <- parallel(subject = nrow(BN), var = ncol(BN), cent = .05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)



A1 <- subset(FW, select=c(AN12, AN1, AN2)) 
A2 <- subset(FW, select=c(AN7, AN5, AN8, AN3)) 
A3 <- subset(FW, select=c(AN4, AN6)) 
A4 <- subset(FW, select=c(AN10, AN9)) 
A5 <- subset(FW, select=c(AN13, AN11)) 


A1 <- na.omit(A1)
A2 <- na.omit(A2)
A3 <- na.omit(A3)
A4 <- na.omit(A4)
A5 <- na.omit(A5)


psych::alpha(AN)
psych::alpha(A2)
psych::alpha(A3)
psych::alpha(A4)
psych::alpha(A5)








cor_halfs <- cor (rowSums(AV),rowSums(AN))
cor_halfs

spearman_brown <- function(reliability, factor) {
  numerator <- reliability * factor
  denominator <- 1 + (factor-1) * reliability
  corrected_reliability <- numerator / denominator
  return(corrected_reliability)
}
split_half <- spearman_brown(cor_halfs, 2)
split_half






save.image("~/R/Rskripte/trunk/FWtest.RData")
