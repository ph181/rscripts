

AN <- as.data.frame(subset(mydata, select=c(F1, F2, F3, F4, F5, F6, F7, F8, F9)))

items <- fa(AN, nfactors=3, SMC=TRUE, fm="ml", rotate="oblimin")
fa.diagram(items)
print(items, digits=2, cutoff=.5, sort=TRUE)

BARTLETT(AN)
KMO(AN)
pairs.panels(AN)
vss(AN)
VSS.scree(AN)
fa.parallel(AN, fa ="fa")
antiImage(AN)
#items <- fa(AN, nfactors=2, SMC=TRUE, fm="pa", scores = TRUE, rotate="promax", cor = "poly" or cor = "tet" or cor = "mixed")



items <- fa(AN, nfactors=2, SMC=TRUE, fm="pa", scores = TRUE, rotate="varimax")
items <- fa(AN, nfactors=3, SMC=TRUE, fm="ml", rotate="oblimin")
items <- fa(AN, nfactors=2, SMC=TRUE, fm="minres", rotate="varimax")
items <- fa(AN, nfactors=2, SMC=TRUE, fm="uls", rotate="varimax")
items <- fa(AN, nfactors=2, SMC=TRUE, fm="gls", rotate="varimax")
items <- fa(AN, nfactors=2, SMC=TRUE, fm="wls", rotate="varimax")





items$STATISTIC
items$PVAL
items$loadings[,]
round(items$Phi, 2) 
barplot(t(loadings(items)), beside = T)
items$loadings[,]
items$Structure[,]
items$Vaccounted


Mahalanobis_Distanz <- mahalanobis(x = Erhebung, cov = cov(Erhebung), center = colMeans(Erhebung)) # Berechnen der Mahalanobisdistanz
hist(Mahalanobis_Distanz, col = "skyblue", border = "blue", freq = F, breaks = 15) # Histogramm
lines(x = seq(0, max(Mahalanobis_Distanz), 0.01), y = dchisq(x = seq(0, max(Mahalanobis_Distanz), 0.01), df = 15), col = "darkblue", lwd = 4) # Einzeichnen der Dichte



 
 mein_modell <- '
# Faktorladungen
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
'
 mein_modell <- '
# Faktorladungen
SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5
'
 
 mein_modell <- '
# Faktorladungen
SKP1 =~ SP1 + SP4 + SP6 + SP7
SKP2 =~ SP2 + SP3 + SP5
'
 mein_modell <- '
# Faktorladungen
SKC1 =~ SC1 + SC2
SKC2 =~ SC3 + SC4 + SC5
SKC3 =~ SC6 + SC7
'
 mein_modell <- '
# Faktorladungen
Flow1=~ F1+F4+ F5+ F6+ F7+ F8+ F9
Flow2 =~ F2+ F3
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
 