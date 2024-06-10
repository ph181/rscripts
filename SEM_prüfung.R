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