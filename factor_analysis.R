




BARTLETT(mydata)
KMO(mydata)
pairs.panels(mydata)
vss(mydata)
VSS.scree(mydata)
fa.parallel(mydata, fa ="fa")
antiImage(mydata)

items <- fa(mydata, nfactors=3, SMC=TRUE, fm="ml", rotate="oblimin")
fa.diagram(items)
print(items, digits=2, cutoff=.5, sort=TRUE)


items$STATISTIC
items$PVAL
items$loadings[,]
round(items$Phi, 2) 
barplot(t(loadings(items)), beside = T)
items$loadings[,]
items$Structure[,]
items$Vaccounted


Mahalanobis_Distanz <- mahalanobis(x = mydata, cov = cov(mydata), center = colMeans(mydata)) # Berechnen der Mahalanobisdistanz
hist(Mahalanobis_Distanz, col = "skyblue", border = "blue", freq = F, breaks = 15) # Histogramm
lines(x = seq(0, max(Mahalanobis_Distanz), 0.01), y = dchisq(x = seq(0, max(Mahalanobis_Distanz), 0.01), df = 15), col = "darkblue", lwd = 4) # Einzeichnen der Dichte



 
 mein_modell <- '
# Faktorladungen
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
'

  fit <- cfa(mein_modell, data = mydata,
            estimator = "MLM",
           # ordered = TRUE,
            meanstructure = TRUE,
            auto.var = TRUE )
 
 summary(fit, fit.measures = TRUE,  rsquare = TRUE)

 
 
semPaths(fit, what = "est")
 semPaths(fit, curve = T, curvePivot = T)
 
 semPaths(object = fit, what = "est", layout = "tree2",
          rotation = 1, curve = T, col = list(man = "skyblue", lat = "yellow"),
          curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)
 
 
 
 semPaths(fit, whatLabels="est", # plot model not parm ests
          rotation = 2, # default rotation = 1 with four options
          asize = 5, # arrows' size
          esize = 2, # width of paths' lines / curves
          edge.label.cex = 0.8, # font size of regr'n coeffs
          sizeMan = 10, # font size of manifest variable names
          nCharNodes = 0,
          nCharEdges = 0, # don't limit variable name lengths
          fade = FALSE, # don't weight path width to reflect strength
          curvePivot = TRUE, # make straight edges instead of round ones
          curve = 2, # pull covariances' curves out a little
          style = "lisrel", # no variances vs. # "ram"'s 2-headed for variances
          color = "grey", # color of variables
          edge.color = "black", # color of edges/paths
          layout = "tree2", # tree, spring, circle, circle2
          residuals = TRUE) # residuals variances included in the path diagram
 
 
 
 lavaanPlot(model=fit, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
            edge_option=list(color="grey"))
 
 mi <- modindices(fit)
 mi[mi$mi > 10,]
 
 ezCutoffs(model = mein_modell, data = Pilotierung)#
 
 mean(1-mean(rowSums(inspect(fit,what="std")$theta)))
 
 
 inspect(fit,what="est")
 parameterEstimates(fit
                    ) 
 inspect(fit, 'r2')
 lavaanify(fit)
 lavInspect(fit, what='free')
 
 
 #categorical
 #warum ich keine PCA durchführe?
 library(polycor)
 library(EFAtools)
 library(psych)
 library(lavaan)
 library(multiUS)
 library(sirt)
 
 library(lavaanPlot)
 
 fa.CFI<-function(x){
   nombre<-paste(x,"CFI",sep = ".")
   nombre<-
     ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
   return(nombre)
 }
 
 df <- sapply(mydata, as.factor)

 het.mat <- hetcor(df)$cor
 BARTLETT(het.mat)
 KMO(het.mat)
 pairs.panels(het.mat)
 vss(het.mat)
 VSS.scree(het.mat)
 fa.parallel(het.mat, fa ="wls") 
 vss(het.mat, rotate = "varimax", diagonal = FALSE, fm = "uls", n.obs=NULL, plot=TRUE,title="Very Simple Structure",use="pairwise",cor="cor")
 nfactors(het.mat,rotate="varimax",diagonal=FALSE,fm="uls",n.obs=NULL, title="Number of Factors",pch=16,use="pairwise")
 
 
 tet.mat <- tetrachoric2( mydata, method="Bo" )$rho
 BARTLETT(tet.mat)
 KMO(tet.mat)
 pairs.panels(tet.mat)
 vss(tet.mat)
 VSS.scree(tet.mat)
 fa.parallel(tet.mat, fa ="uls") 
 vss(tet.mat, rotate = "varimax", diagonal = FALSE, fm = "uls", n.obs=NULL, plot=TRUE,title="Very Simple Structure",use="pairwise",cor="cor")
 nfactors(tet.mat,rotate="varimax",diagonal=FALSE,fm="uls",n.obs=NULL, title="Number of Factors",pch=16,use="pairwise")
 
 
 
 fa.1 <- factanal(covmat = het.mat, factors = 3, rotation = "varimax")
 fa.1
 
 fa.2 <- fa(r = het.mat, nfactors =4, fm = "gls", rotate = "oblimin")
 fa.2
 fa.diagram(fa.2)
 fa.CFI(fa.2)
 
 fa.3 <- fa(r = tet.mat, nfactors =3
            ,fm = "gls"
            , rotate = "varimax"
            #           ,rotate = "oblimin"
            #           ,rotate = "promax"
 )
 fa.3
 fa.diagram(fa.3)
 fa.CFI(fa.3)
 
 mein_modell <- '
# Faktorladungen
#GLS 1.00 DWLS
G1 =~ AN1+AN2+AN3+AN4+AN8
G2 =~ AN5+AN7+AN9+AN10+AN12
G3 =~ AN11+AN13

#G1 =~ AN1+AN2+AN3
#G2 =~ AN4+AN6+AN5
#G3 =~ AN12+AN7+AN9+AN8


#u1 =~ BN1+BN2+BN8+BN11
#u2 =~ BN13+BN12+BN6+BN7+BN9
#u3 =~ BN5+BN4
#u4 =~ BN10+BN3
#G5 =~ BN13+BN6


#AN9  ~~ AN12
#AN4  ~~ AN6
 '
 
 fit <- cfa(mein_modell, data = tet.mat,
            estimator = "dwls",
            ordered = TRUE
 )
 summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
 mi <- modindices(fit)
 mi[mi$mi > 10,]
 lavaanPlot(model=fit, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
            edge_option=list(color="grey"))
 
 
 omegaFromSem(fit,m=NULL,flip=TRUE,plot=TRUE)
 
 semPaths(fit, whatLabels="est", # plot model not parm ests
          rotation = 2, # default rotation = 1 with four options
          asize = 5, # arrows' size
          esize = 2, # width of paths' lines / curves
          edge.label.cex = 0.8, # font size of regr'n coeffs
          sizeMan = 10, # font size of manifest variable names
          nCharNodes = 0,
          nCharEdges = 0, # don't limit variable name lengths
          fade = FALSE, # don't weight path width to reflect strength
          curvePivot = TRUE, # make straight edges instead of round ones
          curve = 2, # pull covariances' curves out a little
          style = "lisrel", # no variances vs. # "ram"'s 2-headed for variances
          color = "grey", # color of variables
          edge.color = "black", # color of edges/paths
          layout = "tree2", # tree, spring, circle, circle2
          residuals = TRUE) # residuals variances included in the path diagram
 
 
 semPaths(fit, what = "est")
 
 semPaths(fit, curve = T, curvePivot = T)
 
 semPaths(object = fit, what = "est", layout = "tree2",
          rotation = 1, curve = T, col = list(man = "skyblue", lat = "yellow"),
          curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)
 
 
 
 G1 <- subset (FW, select=c(AN1,AN3,AN8))
 G2 <- subset (FW, select=c(AN5,AN7,AN9,AN12,AN10))
 G3 <- subset (FW, select=c(AN11, AN4,AN6))
 kr20(G1)
 kr20(G2)
 kr20(G3)
 
 
 mein_modell <- '
# ULS

 '
 
 fit <- cfa(mein_modell, data = FW,
            #           estimator = "ULS",
            ordered = TRUE, )
 
 summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
 
 
 