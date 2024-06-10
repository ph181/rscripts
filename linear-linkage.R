#linear lnkinkg
library(readxl)
FW <- read_excel("Erhebung.xlsx", sheet = "FW")
View(FW)

library(equate)

colMeans(AN)
colMeans(BV)


AN <- as.data.frame(subset(Erhebung, AN1])))
AN
SKP <- SKP[complete.cases(SKP),]

a <- as.array(colMeans(AN))
rowMeans(a)

AV <-   subset(Erhebung, select=c(AV1, AV2, AV3, AV4, AV5, AV7, AV8,  AV11, AV12, AV13))
BV <-  subset(Erhebung, select=c(BV1, BV2, BV3, BV4, BV5, BV7, BV10, BV11, BV12, BV13))


AV <- AV[complete.cases(AV),]
AV <- as.freqtab(AV)

BN <- BV[complete.cases(BV),]
BV <- as.freqtab(BV)

equate(AV, BV, type = "equipercentile", method = "frequency estimation" )

AM <-  subset(Erhebung, select = c(AN1, AN2, AN3, AN4, AN5, AN7, AN8, AN11, AN12, AN13))
BM <-  subset(Erhebung, select = c(BN1, BN2, BN3, BN4, BN5, BN7, BN10, BN11, BN12, BN13))

AN <- AM[complete.cases(AM),]
AN <- as.freqtab(AN)

BN <- BM[complete.cases(BM),]
BN <- as.freqtab(BN)


equate(AN, BN, type = "equipercentile", method = "frequency estimation" )


equate(
  AN,
  BN,
  type = c(#"identity"
           "mean"
           #"linear"
           #"general linear"
           #"circle-arc"
           #"equipercentile"
           ),
  method = c("none" 
             #"nominal weights" 
             #"tucker"
             #"levine" 
             #"frequency estimation"
             #"chained" 
             #"braun/holland"
             ),
  #name,
  #lowp,
  #highp,
  #boot = FALSE,
  #verbose = TRUE,
  )



