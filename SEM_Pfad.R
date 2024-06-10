library(lavaan)
library(tidySEM)
library(semPlot)
library(ezCutoffs)
library(lavaanPlot)
library(psych)

#https://link.springer.com/chapter/10.1007/978-3-658-16936-7_30
#https://bookdown.org/luenich/Latente_Variablen/befehle-kfa.html

head(PC)
swe <- Erhebung
PC <- swe
PC$G = as.factor(PC$G)

#dummy<-dummy.code(PC$G) 

head(dummy)

mein_modell <- '
# Faktorladungen
#SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
#SKC =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC7
#Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3
#SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
#SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5


#FWAV =~ AV1 + AV2 + AV3 + AV4 + AV5 + AV6 + AV7 + AV8 + AV9 + AV10 + AV11 + AV12 + AV13
#FWBV =~ BV1 + BV2 + BV3 + BV4 + BV5 + BV6 + BV7 + BV8 + BV9 + BV10 + BV11 + BV12 + BV13
#FWAN =~ AN1 + AN2 + AN3 + AN4 + AN5 + AN6 + AN7 + AN8 + AN9 + AN10 + AN11 + AN12 + AN13
#FWBN =~ BN1 + BN2 + BN3 + BN4 + BN5 + BN6 + BN7 + BN8 + BN9 + BN10 + BN11 + BN12 + BN13


#Strukturmodell/Regression
#SKP ~ N + G + FV
#SKC ~ N + G  + FV
#FV ~ SKP + SKC + N

#alle Gweschlechter
FL ~ SP + FV + SC
#Flow ~ SKP + SKC +FV

#w
FL ~ SC
#FN~SP+FL+FV
#DF ~ SP + FL


#m
#FL ~ SP + FV

##hayes4
##direct effect
#FN ~ c*FV + FL
#direct := c

##regressions
#SP ~ a*FV
#FN ~ b*SP

##indirect effect (a*b)
#indirect := a*b

##total effect
#total := c + (a*b)

##Prop
#prop := indirect/total


#Flow ~ SKP + SKC
#FN ~ SKP + SKC + Flow + FV + N
#DF ~ SKP + SKC + Flow + FV + N
#FWAN ~ SKP + SKC + Flow+ SWEV + FWAV

#Kov
#SC2 ~~  SC5
#SC4 ~~  SC6
#SC6 ~~  SC7
SP2 ~~ SP3 
#SKP ~~  SKC

#SKP ~~  FV
#SKC ~~  FV
F7  ~~  F8

'

model_fit <- sem(data = Erhebung, model = mein_modell
#                 , missing = "ML"
#                 ,group = "G" 
                 )


#summary(model_fit, fit.measures = TRUE)
#inspect(model_fit, 'rsquare')
#lavInspect(model_fit, "cov.lv")
# Standardisiertes Ergebnis und R²
summary(model_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
#parameterEstimates(model_fit, ci = TRUE)
#lavInspect(model_fit, "cov.lv")

mi <- modindices(model_fit)
mi[mi$mi > 10,]

#ezCutoffs(model = mein_modell, data = Erhebung)

scores <- predict(model_fit)
head(scores)
#https://gansser.de/post/strukturgleichungsmodellierung-anleitung/
fitMeasures(model_fit, c("chisq", "df", "rmsea", "gfi", "agfi", "rmr", "srmr", "nfi", "nnfi", "cfi" ))

# Visualisierung mit tidySEM
pfad_layout <- get_layout("", "", "",
                          "SV", "", "",
                          "SC", "", "DS",
                          "FV", "FL", "",
                          "SP", "", "DF", rows = 5)

graph_sem(model = model_fit, layout = pfad_layout) 


semPaths(model_fit, what = "est")
semPaths(model_fit, curve = T, curvePivot = T)

semPaths(object = model_fit, what = "est", layout = "tree2",
         rotation = 1, curve = T, col = list(man = "skyblue", lat = "yellow"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)



semPaths(model_fit, whatLabels="est", # plot model not parm ests
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



lavaanPlot(model=model_fit, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
           edge_option=list(color="grey"))

# Modell nach Modifikation
mein_modell2 <- '
# Faktorladungen
SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
SKC =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC7
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3
SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5


#Strukturmodell/Regression
SKP ~ N# + G +L
SKC ~ N# + G +L
SWEV ~ N# + #G

Flow ~ SKP + SKC + SWEV + N

SWEN ~ SKP + SKC + Flow + SWEV + DF

#alle Gweschlechter
FL ~ SP + SC + FV

#w
#FL ~ SC

#m
#FL ~ SP + FV
#DS ~ Flow + SKP + SKC + SWEV + FV

#kovs 
#F2 ~~  F3
#F7 ~~  F8
F2 ~~  F8

SC2 ~~  SC5
SC4 ~~  SC6
SC6 ~~  SC7

SP2 ~~ SP3 

SKP ~~ SWEV
#SKC ~~ SWEV
 
SKP ~~  SKC

#SWEV ~~  FV
#SKP ~~  FV
#SKC ~~  FV
# 
#SKC ~~ Flow
#SKP ~~ Flow
#SWEV ~~ Flow
#FV ~~ Flow
# 
# SWEV ~~  FN
# SKP ~~  FN
# SKC ~~  FN
# Flow ~~  FN
SWEV ~~  SWEN
# SKP ~~  SWEN
# SKC ~~  SWEN
# Flow ~~  SWEN
'

model_fit2 <- sem(data = swe, model = mein_modell2, missing = "ML")

summary(model_fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Vergleich der beiden Modelle
#lavTestLRT(model_fit, model_fit2)


mi <- modindices(model_fit2)
mi[mi$mi > 10,]

ezCutoffs(model = mein_modell2, data = Erhebung)


semPaths(model_fit2, what = "est")

semPaths(model_fit2, curve = T, curvePivot = T)

semPaths(object = model_fit2, what = "est", layout = "tree2",
         rotation = 4, curve = T, col = list(man = "skyblue", lat = "yellow"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)

lavaanPlot(model=model_fit2, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
           edge_option=list(color="grey"))


# Visualisierung mit tidySEM
pfad_layout <- get_layout("SKC", "", "",
                          "SKP", "", "",
                          "SWEV", "", "SWEN",
                         "", "Flow", "",
                         "FV", "", "FN", rows = 5)

graph_sem(model = model_fit2, layout = pfad_layout) 


# Modell nach Modifikation
mein_modell3 <- '
# Faktorladungen
#SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
#SKC =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC7
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5
Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3


#FWAV =~ AV1 + AV2 + AV3 + AV4 + AV5 + AV6 + AV7 + AV8 + AV9 + AV10 + AV11 + AV12 + AV13
#FWBV =~ BV1 + BV2 + BV3 + BV4 + BV5 + BV6 + BV7 + BV8 + BV9 + BV10 + BV11 + BV12 + BV13
#FWAN =~ AN1 + AN2 + AN3 + AN4 + AN5 + AN6 + AN7 + AN8 + AN9 + AN10 + AN11 + AN12 + AN13
#FWBN =~ BN1 + BN2 + BN3 + BN4 + BN5 + BN6 + BN7 + BN8 + BN9 + BN10 + BN11 + BN12 + BN13


#Strukturmodell/Regression
#SKP ~ N + L + G
#SKC ~ N + L + G
#SWEV ~ N + L + G
#FV ~ N + L + G
#Flow ~ SKP + SKC + SWEV + FV

Flow ~ SP + SC + SV + FV

#SWEN ~ SKP + SKC + FL + SWEV + FV
#FN ~ SKP + SKC + FL+ SWEV + FV

SWEN ~ SP + SC + Flow + DS
#FN ~ SKP + SKC + FL+ DF

#DF ~ Flow + SKP + SKC + SWEV + FV
#DS ~ Flow + SKP + SKC + SWEV + FV

#kovs 
#F2 ~~  F3
#F7 ~~  F8
F2 ~~  F8

#SC2 ~~  SC5
#SC4 ~~  SC6
#SC6 ~~  SC7

#SP2 ~~ SP3 

#SKP ~~ SWEV
#SKC ~~ SWEV
 
#SKP ~~  SKC

#SWEV ~~  FV
#SKP ~~  FV
#SKC ~~  FV
# 
#SKC ~~ Flow
#SKP ~~ Flow
#SWEV ~~ Flow
#FV ~~ Flow
# 
# SWEV ~~  FN
# SKP ~~  FN
# SKC ~~  FN
# Flow ~~  FN
# SWEV ~~  SWEN
# SKP ~~  SWEN
# SKC ~~  SWEN
# Flow ~~  SWEN
'

model_fit3 <- sem(data = PC, model = mein_modell3)
                  #group = "G" =1)

summary(model_fit3, fit.measures = TRUE)

mi <- modindices(model_fit3)
mi[mi$mi > 10,]

ezCutoffs(model = mein_modell3, data = Erhebung)

#Vergleich der beiden Modelle
lavTestLRT(model_fit3, model_fit3)

semPaths(model_fit3, what = "est")

semPaths(model_fit3, curve = T, curvePivot = T)

semPaths(object = model_fit3, what = "est", layout = "tree2",
         rotation = 4, curve = T, col = list(man = "white", lat = "grey"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)


semPaths(model_fit3, whatLabels="est", # plot model not parm ests
         rotation = 4, # default rotation = 1 with four options
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
