

install.packages("foreign", dependencies=TRUE)


library(foreign) 
library(lavaan)
library(tidySEM)
library(semPlot)
library(ezCutoffs)
library(lavaanPlot)
library(psych)





#one factor three items, default marker method
mein_modell1  <- ' 
SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
'

mein_modell2  <- ' 
S =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7 + SV1 + SV2 + SV3 + SV4 + SV5
'



fit1 <- cfa(mein_modell1, data = Erhebung,
           estimator = "MLM",
           #ordered = TRUE,
     #      meanstructure = TRUE,
     #      auto.var = TRUE 
     #std.lv = TRUE
     )
fit2 <- cfa(mein_modell2, data = Erhebung,
           estimator = "MLM",
           #ordered = TRUE,
           #      meanstructure = TRUE,
           #      auto.var = TRUE 
           #std.lv = TRUE
)


summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


anova(fit1,fit2)


cor(Erhebung$SV, Erhebung$SP)

fitMeasures(fit, c("chisq", "df", "rmsea", "gfi", "agfi", "rmr", "srmr", "nfi", "nnfi", "cfi" ))

mi <- modindices(fit)
mi[mi$mi > 10,]


semPaths(fit, what = "est")
semPaths(fit, curve = T, curvePivot = T)

semPaths(object = fit, what = "est", layout = "tree2",
         rotation = 1, curve = T, col = list(man = "skyblue", lat = "yellow"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)



la <- semPaths(fit, whatLabels="Std.lv", # plot model not parm ests
         rotation = 2, # default rotation = 1 with four options
         asize = 3, # arrows' size
         esize = 3, # width of paths' lines / curves
         edge.label.cex = 1.2, # font size of regr'n coeffs
         sizeMan = 6, # font size of manifest variable names
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



la <- semPaths(fit, whatLabels="est", # plot model not parm ests
               rotation = 2, # default rotation = 1 with four options
               asize = 5, # arrows' size
               esize = 2, # width of paths' lines / curves
               edge.label.cex = 1, # font size of regr'n coeffs
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
               residuals = TRUE,               
               intercepts = FALSE) # residuals variances included in the path diagram



le <- mark_sig(
  la,
  fit,
  alphas = c(`*` = 0.05, `**` = 0.01, `***` = 0.001)
)
plot(le)



lavaanPlot(model=fit, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
           edge_option=list(color="grey"))

mi <- modindices(fit)
mi[mi$mi > 10,]

ezCutoffs(model = mein_modell, data = Pilotierung)

