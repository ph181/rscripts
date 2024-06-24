
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


mydata <- pmm

clust <- as.matrix(mydata)

describe(clust)




#If the quantitative and qualitative data are in a same dataframe, the function PCAmixdata::splitmix 
#can be used to extract automatically the qualitative and the quantitative data in two separated dataframes. 
#xquant <- AN[,c(1,2,5,7,8)] # Numeric variables
#xqual <- AN[,c(3,4,6)]      # Categorical variables
#tree <- hclustvar(xquant, xqual)

tree <- hclustvar(clust,)
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

KMO(AN)

BARTLETT(AN)

nfactors(AN, rotate = "varimax", fm = "mle")



ev <- eigen(cor(AN))
ap <- parallel(subject = nrow(AN), var = ncol(AN), cent = .05)
nS <- nScree(x=ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)




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
