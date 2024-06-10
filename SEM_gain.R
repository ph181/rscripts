library(lavaan)
library(tidySEM)
library(semPlot)
library(ezCutoffs)
library(lavaanPlot)
library(psych)
library(semptools)
library(semTable)
library(report)
library(semTools)
#install.packages("report")


#https://link.springer.com/chapter/10.1007/978-3-658-16936-7_30
#https://bookdown.org/luenich/Latente_Variablen/befehle-kfa.html


Erhebung$C = as.factor(Erhebung$C)

mydata <- Erhebung[which(Erhebung$C ==0),]

mydata <- Erhebung


mein_modell <- '
# Faktorladungen
SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
SKC =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC7
Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
#SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5

#Flow ~ C
#FN ~ C
#SWEN ~ C

SKP ~  G
SKC ~  G
FV ~  G
SWEV ~  G

Flow ~ G
Flow ~ SKP
Flow ~ SKC
Flow ~ FV
Flow ~ SWEV

DFp ~ G
DFp ~ Flow
DFp ~ SKP
DFp ~ SKC
DFp ~ SWEV

DSp ~ G
DSp ~ Flow
DSp ~ SKP
DSp ~ SKC
DSp ~ FV


# FLo ~ G
# FLo ~ SKP
# FLo ~ SKC
# FLo ~ FV
# FLo ~ SWEV

# FN ~ G
# #FN ~ FLo:FV
# FN ~ Flow
# FN ~ SKP
# FN ~ SKC
# FN ~ FV
# FN ~ SWEV

# SWEN ~ G
# SWEN ~ Flow
# SWEN ~ SKP
# SWEN ~ SKC
# SWEN ~ FV
# SWEN ~ SWEV


#mediator
#Flow ~ a*SWEV

# #direct effect
# SWEN ~ c*SWEV
# SWEN ~ b*Flow
# 
# #indirect effect (a*b)
# ab := a*b
# #total effect
# total := c + (a*b)


# #Kov
SKP ~~ SWEV
DFp ~~FV
DSp ~~ SWEV
F2 ~~ F8
F2 ~~ F3
SP2 ~~ SP3
SC2 ~~SC5
SC4~~SC6
SP5 ~~ SV5
SP7 ~~SV4
SP7 ~~ SV5
SC3 ~~ SV2
SC5 ~~ SV3
SP7 ~~ F4
SC5 ~~ F8
'

model_fit <- sem(data = mydata, model = mein_modell
#, missing = "ML"
#,group ="C"
#, "G"
#,group = "C"
#,test="bollen.stine"
#,se="bootstrap", bootstrap = 5000
)

#lavInspect(model_fit, "cov.lv")
#a <- compareFit(model_fit_a, model_fit_b)
#summary(a)
#summary(model_fit, fit.measures = TRUE)
#inspect(model_fit, 'rsquare')
#lavInspect(model_fit, "cov.lv")
# Standardisiertes Ergebnis und R²
summary(model_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
mi <- modindices(model_fit)
mi[mi$mi > 10,]


inspect(model_fit,"rsquare")

summary(model_fit)


a <- semTable(model_fit, file = NULL, paramSets = "all", 
         columns = c(est = "Std.all", se = "SE", z = "z", p = "p"),
         varLabels = NULL, groups = NULL,
          table.float = FALSE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001), type = "html")


suppressWarnings(report_table(model_fit))
suppressWarnings(report_performance(model_fit))
#parameterEstimates(model_fit, boot.ci.type ="bca.simple", level=.95, output="pretty", header=TRUE) 
#parameterEstimates(model_fit, ci = TRUE)
#lavInspect(model_fit, "cov.lv")
ezCutoffs(model = mein_modell, data = mydata)
#https://gansser.de/post/strukturgleichungsmodellierung-anleitung/
fitMeasures(model_fit, c("chisq", "df", "rmsea", "gfi", "agfi", "rmr", "srmr", "nfi", "nnfi", "cfi" ))

pfad_layout <- get_layout("","SKP", "", "",
                          "","FV", "", "DSp",
                          "G","", "Flow", "",
                          "","SWEV", "", "DFp",
                          "","SKC", "", "", rows = 5)

graph_sem(model = model_fit, layout = pfad_layout, angle = NULL, spacing_x = 5, spacing_y = 4, variance_diameter = 0.5, text_size = 3) 

run_lavaan(measurement(tidy_sem(mydata), meanstructure = TRUE))

la <- semPaths(model_fit, what = "Std.all", nCharNodes = 0,
               nCharEdges = 0,
               sizeMan = 10, # font size of manifest variable names
               sizeLat = 10,
               edge.label.cex = 1)

la <- semPaths(model_fit,  what = "Std.all",curve = T, curvePivot = T)

la <- semPaths(object = model_fit, what = "est", layout = "tree2",
         rotation = 1, curve = T, col = list(man = "skyblue", lat = "yellow"),
         edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)

la <- semPaths(model_fit, whatLabels="Std.lv", # plot model not parm ests
               rotation = 2, # default rotation = 1 with four options
               asize = 1, # arrows' size
               esize = 1, # width of paths' lines / curves
               edge.label.cex = 1, # font size of regr'n coeffs
               sizeMan = 5,
               sizeLat = 5, # font size of manifest variable names
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



la <- semPaths(model_fit, whatLabels="est", # plot model not parm ests
         #rotation = 2, # default rotation = 1 with four options
         #asize = 2, # arrows' size
         #esize = 2, # width of paths' lines / curves
        # edge.label.cex = 1, # font size of regr'n coeffs
         sizeMan = 5, # font size of manifest variable names
         sizeLat = 5,
         nCharNodes = 0,
         nCharEdges = 0, # don't limit variable name lengths
         fade = TRUE, # don't weight path width to reflect strength
         curvePivot = TRUE, # make straight edges instead of round oneshttp://127.0.0.1:17169/graphics/plot_zoom_png?width=1308&height=720
         curve = 2, # pull covariances' curves out a little
         style = "lisrel", # no variances vs. # "ram"'s 2-headed for variances
         color = "grey", # color of variables
         edge.color = "black", # color of edges/paths
         layout = "tree2", # tree, spring, circle, circle2
         residuals = TRUE,               
         intercepts = TRUE) # residuals variances included in the path diagram



le <- mark_sig(
  la,
  model_fit,
  alphas = c(`*` = 0.05, `**` = 0.01, `***` = 0.001)
)
plot(le)

lavaanPlot(model=model_fit, coefs=T, cov=T, stand = T, stars = c ("covs", "latent","regress"),
           edge_option=list(color="grey"))
