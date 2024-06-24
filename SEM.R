#prüfung
library(lavaan)

# 1. Test auf Normalverteilung

install.packages("MVN")
library(MVN)

#Auswahl Variablen
daten_mv_test <- subset(Erhebung, select = c(SP1,SP2,SP3,SP4,SP5,SP6,SP7, SP, SC1,SC2,SC3,SC4,SC5,SC6,SC7,SC, SV1,SV2,SV3,SV4,SV5,SN1,SN2,SN3,SN4,SN5,FV,FN,F1,F2,F3,F4,F5,F6,F7,F8,F9, FL)) 
daten_mv_test <- subset(Erhebung, select = c(SP,SC, SV, SN, FV,FN,FL)) 
#Test auf multivariate Normalverteilung
mvn(daten_mv_test, mvnTest="mardia")
mvn(daten_mv_test, mvnTest="mardia", univariateTest = "SW")

# 2. Robuster Test nach Satorra-Bentler
strukturmodell <- '
# Faktorladungen
SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
SKC =~ SC1 + SC2 + SC3 + SC4 + SC5 + SC6 + SC7
SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3
SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5


#Strukturmodell/Regression
SKP ~  G
SKC ~  G
SWEV ~  G
FV ~  G
Flow ~ G
SWEN ~ G
FN ~ G

Flow ~ SKP + SKC + SWEV + FV

SWEN ~ Flow + SKP + SKC + SWEV + FV
FN ~ Flow + SKP + SKC + SWEV + FV




#Kovarianzen



'

model_fit_r <- sem(data = mydata, model = strukturmodell,
                   estimator="MLM")

summary(model_fit_r, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# 3. Robuster Test mit Bootstrapping

model_fit_b <- sem(data = mydata, model = strukturmodell,
                   test="bollen.stine", se="bootstrap", bootstrap = 5000)

# Ergebnis mit Bootstrapping
summary(model_fit_b, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

parameterEstimates(model_fit_b, boot.ci.type ="bca.simple", level=.95,
                   output="pretty", header=TRUE) 


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



mydata <- Erhebung[which(Erhebung$C ==0),]


mein_modell <- '
# Faktorladungen
VarA =~ VA1 + VA2 + VA3 + VA4 + VA5
VarB =~ VB1 + VB2 + VB3 + VB4 + VB5



Flow ~ VarA + VarB


#Kov
VA1 ~~VB1
'

model_fit <- sem(data = mydata, model = mein_modell,fixed.x = FALSE
                 , missing = "ML"
                 #,group ="Group"
                 #,test="bollen.stine"
                 #,se="bootstrap", bootstrap = 5000
)


summary(model_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


mi <- modindices(model_fit)
mi[mi$mi > 10,]

inspect(model_fit,"rsquare")


suppressWarnings(report_table(model_fit))
suppressWarnings(report_performance(model_fit))
#parameterEstimates(model_fit, boot.ci.type ="bca.simple", level=.95, output="pretty", header=TRUE) 
#parameterEstimates(model_fit, ci = TRUE)
ezCutoffs(model = mein_modell, data = mydata)
#fitMeasures(model_fit, c("chisq", "df", "rmsea", "gfi", "agfi", "rmr", "srmr", "nfi", "nnfi", "cfi" ))

pfad_layout <- get_layout("","", "", "",
                          "","", "", "",
                          "","", "", "", rows = 5)

graph_sem(model = model_fit, layout = pfad_layout, angle = NULL, spacing_x = 5, spacing_y = 4, variance_diameter = 0.5, text_size = 3) 


graph_sem(
  model = model_fit,
  layout = pfad_layout,
  nodes = NULL,
  rect_width = 1.2,
  rect_height = 0.8,
  ellipses_width = 1,
  ellipses_height = 1,
  variance_diameter = 0.2,
  spacing_x = 2,
  spacing_y = 2,
  text_size = 4,
  curvature = 60,
  angle = 0,
  fix_coord = FALSE,
  
)



run_lavaan(measurement(tidy_sem(mydata), meanstructure = TRUE))

la <- semPaths(model_fit, what = "Std.all", nCharNodes = 0,
               nCharEdges = 1,
               sizeMan = 5, # font size of manifest variable names
               sizeLat = 5,
               edge.label.cex = 0.5)

la <- semPaths(model_fit,  what = "Std.all",curve = T, curvePivot = T)

la <- semPaths(object = model_fit, 
               what = "est", 
               layout = "tree2",
               rotation = 3, 
               curve = T, 
               # col = list(man = "skyblue", lat = "yellow"),
               edge.label.cex=0.4, 
               sizeMan = 4, 
               sizeLat = 4,
               nCharNodes = 0
)

la <- semPaths(model_fit, whatLabels="Std.lv", # plot model not parm ests
               rotation = 1, # default rotation = 1 with four options
               asize = 1, # arrows' size
               esize = 1, # width of paths' lines / curves
               edge.label.cex = 0.7, # font size of regr'n coeffs
               sizeMan = 4,
               sizeLat = 5, # font size of manifest variable names
               nCharNodes = 0,
               nCharEdges = 0, # don't limit variable name lengths
               fade = FALSE, # don't weight path width to reflect strength
               curvePivot = TRUE, # make straight edges instead of round ones
               curve = 1, # pull covariances' curves out a little
               style = "lisrel", # no variances vs. # "ram"'s 2-headed for variances
               color = "grey", # color of variables
               edge.color = "black", # color of edges/paths
               layout = "tree2", # tree, spring, circle, circle2
               residuals = TRUE,               
               intercepts = FALSE) # residuals variances included in the path diagram



la <- semPaths(model_fit, whatLabels="Std.lv", # plot model not parm ests
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
               curve = 1, # pull covariances' curves out a little
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

lavaanPlot(model=model_fit, coefs=T, stand = T, stars = c ("covs", "latent","regress"),
           edge_option=list(color="grey"))
