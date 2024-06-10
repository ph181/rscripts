library(psych)
library(testing)
library(lavaan)
library(sirt)
library(polycor)
library(EFAtools)

AM <- as.data.frame(subset(Erhebung, select = c(AN1, AN2, AN3, AN4, AN5, AN6, AN7, AN8, AN9, AN10, AN11, AN12, AN13)))

BM <-  as.data.frame(subset(Erhebung, select = c(BN1, BN2, BN3, BN4, BN5, BN6, BN7, BN8, BN9, BN10, BN11, BN12, BN13)))

AN <- AM[complete.cases(AM),]
BN <- BM[complete.cases(BM),]

colMeans(AN)
sapply(AN,sd)

colMeans(BN)
sapply(BN,sd)

tet.mat <- tetrachoric2( AN, method="Bo" )$rho
tet.mat <- tetrachoric2( BN, method="Bo" )$rho

omega(tet.mat)

omega(AM)
omega (BM)

omegah(tet.mat,fm="pc",rotate="oblimin")
omega(tet.mat,3, fm="pc",rotate="promax")
omegaSem(tet.mat, nfactors=3,fm="pc",key=NULL,flip=TRUE,digits=2,title="Omega",
         sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate="oblimin",
         Phi = NULL, option="equal",lavaan=TRUE)
