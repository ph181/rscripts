library(psych)
library(testing)
library(lavaan)


install.packages("Rtools")
# Cronbachs Alpha: Testen der Voraussetzungen

mod1 <- 'ETA=~ p1*AN1 + p1*AN2 + p1*AN3 + p1*AN4 + p1*AN5 + p1*AN6 + p1*AN7 + p1*AN8 + p1*AN9 + p1*AN10 + p1*AN11 + p1*AN12 + p1*AN13 

# Den Fehlervarianzen werden unterschiedliche Labels (e1 bis e8) zugewiesen
AN1~~e1*AN1
AN2~~e2*AN2
AN3~~e3*AN3
AN4~~e4*AN4
AN5~~e5*AN5
AN6~~e6*AN6
AN7~~e7*AN7
AN8~~e8*AN8
AN9~~e9*AN9
AN10~~e10*AN10
AN11~~e11*AN11
AN12~~e12*AN12
AN13~~e13*AN13
'
# Analyse des Modells mod1
# ML-Schätzung (estimator= "ML")
# Die latente Varianz wurde auf eins fixiert (std.lv=TRUE)
fit.mod1 <- sem(mod1, data= FW, estimator= "ML", std.lv=TRUE)
# Ergebnisse anzeigen
summary(fit.mod1)



mein_modell <- '
# Faktorladungen
#SKP =~ SP1 + SP2 + SP3 + SP4 + SP5 + SP6 + SP7
#SP2 ~~ SP3
#SP1 ~~ SP3 


#SKC =~ SC1 + SC2 + SC3+ SC4+ SC5+ SC6+ SC7
#SC2 ~~  SC5 
#SC4 ~~  SC6
#SC6 ~~  SC7


#SWEV =~ SV1 + SV2 + SV3 + SV4 + SV5
#SWEN =~ SN1 + SN2 + SN3 + SN4 + SN5


#Flow =~ F1 + F2  + F4 + F5 + F6 + F7 + F8 + F9 + F3
#F2 ~~   F8
#F2 ~~   F3


#FWAV =~ AV1 + AV2 + AV3 + AV4 + AV5 + AV6 + AV7 + AV8 + AV9 + AV10 + AV11 + AV12 + AV13
#FWBV =~ BV1 + BV2 + BV3 + BV4 + BV5 + BV6 + BV7 + BV8 + BV9 + BV10 + BV11 + BV12 + BV13
#FWAN =~ AN1 + AN2 + AN3 + AN4 + AN5 + AN6 + AN7 + AN8 + AN9 + AN10 + AN11 + AN12 + AN13

#FWBN =~ BN1 + BN2 + BN3 + BN4 + BN5 + BN6 + BN7 + BN8 + BN9 + BN10 + BN11 + BN12 + BN13

#FWAV =~ AV1 + AV2 + AV3 + AV5 + AV7 + AV8 + AV12
#FWBV =~ BV2 + BV4 + BV5 + BV7 + BV8 + BV9 + BV12
FWAN =~ AN1 + AN2 + AN3 + AN5 + AN7 + AN8 + AN12
#FWBN =~ BN2 + BN4 + BN5 + BN7 + BN8 + BN9 + BN12

'

model_fit <- sem(data = X22, model = mein_modell)
summary(model_fit, fit.measures = TRUE)
ezCutoffs(model = mein_modell, data = FW)

mi <- modindices(model_fit)
mi[mi$mi > 10,]

SKP <- as.data.frame(subset(Erhebung[,paste0("SP",1:7)]))
SKP <- SKP[complete.cases(SKP),]
#SKP<-round(SKP,2)
psych::alpha(SKP, check.keys = TRUE)
#Ergebnisse
colMeans(SKP)
#trennschärfe
SKP$score <- rowSums(SKP)
round(cor(SKP), 2)


SKC <- as.data.frame(subset(Erhebung[,paste0("SC",1:7)]))
SKC <- SKC[complete.cases(SKC),]
#SKC<-round(SKC,2)
psych::alpha(SKC, check.keys = TRUE)
#rowSums(subset(PC[,paste0("SC",1:7)]))
#Ergebnisse
colMeans(SKC)
#trennschärfe
SKC$score <- rowSums(SKC)
round(cor(SKC), 2)


SWEV <- as.data.frame(subset(Erhebung[,paste0("SV",1:5)]))
SWEV <- SWEV[complete.cases(SWEV),]
#SWEV<-round(SWEV,2)
psych::alpha(SWEV, check.keys = TRUE)
#rowSums(subset(PC[,paste0("SV",1:5)]))
#Ergebnisse
colMeans(SWEV)
#trennschärfe
SWEV$score <- rowSums(SWEV)
round(cor(SWEV), 2)


SWEN <- as.data.frame(subset(Erhebung[,paste0("SN",1:5)]))
SWEN <- SWEN[complete.cases(SWEN),]
#SWEN<-round(SWEN,2)
psych::alpha(SWEN, check.keys = TRUE)
#rowSums(subset(PC[,paste0("SN",1:5)]))
#Ergebnisse
colMeans(SWEN)
#trennschärfe
SWEN$score <- rowSums(SWEN)
round(cor(SWEN), 2)


Flow <- as.data.frame(subset(Erhebung[,paste0("F",1:9)]))
Flow <- Flow[complete.cases(Flow),]
#Flow<-round(Flow,2)
psych::alpha(Flow, check.keys = TRUE)
#rowSums(subset(PC[,paste0("F",1:9)]))
#Ergebnisse
colMeans(Flow)
#trennschärfe
Flow$score <- rowSums(Flow)
round(cor(Flow), 2)


AV <- as.data.frame(subset(FW, select=c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13)))
AV <- na.omit(AV)
AV <- as.matrix(AV)
kr20(AV)
drop_item(AV, cor.method = c("pearson", "biserial", "polyserial"), fn = sum)


BV <- as.data.frame(subset(FW, select=c(BV1, BV2, BV3, BV4, BV5, BV6, BV7, BV8, BV9, BV10, BV11, BV12, BV13)))
BV <- na.omit(BV)
BV <- as.matrix(BV)
kr20(BV)
#http://127.0.0.1:31089/graphics/plot_zoom_png?width=1200&height=900
drop_item(BV, cor.method = c("pearson", "biserial", "polyserial"), fn = sum)


AM <- as.data.frame(subset(Erhebung, select = c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13, AN1, AN2, AN3, AN4, AN5, AN6, AN7, AN8, AN9, AN10, AN11, AN12, AN13)))
AM <- as.data.frame(subset(Erhebung, select = c(AN1, AN2, AN3, AN4, AN5, AN7, AN8, AN11, AN12)))
A <- AM[complete.cases(AM),]
A <- as.matrix(A)
drop_item(A, cor.method = c("pearson", "biserial", "polyserial"), fn = sum)
split_half(A)
omegah(A)
omega(A,1)
psych::alpha(A, check.keys = TRUE)

BM <- as.data.frame(subset(Erhebung, select = c(BV1, BV2, BV3, BV4, BV5, BV7, BV10, BV11, BV12, BV13)))
BM <- as.data.frame(subset(Erhebung, select = c(BN1, BN2, BN3, BN4, BN5, BN7, BN10, BN11, BN12, BN13)))
B <- na.omit(BM)
B <- as.matrix(B)
kr20(B)
drop_item(B, cor.method = c("pearson", "biserial", "polyserial"), fn = sum)
split_half(B)
omegah(B)
omega(B,1)
psych::alpha(B, check.keys = TRUE)




kr20(AN)
drop_item(AN, cor.method = c("pearson", "biserial", "polyserial"), fn = sum)
split_half(AN)
omegah(tet.mat, fm="pc")
omega(tet.mat,3, fm="pc")
omegaSem(tet.mat, nfactors=3,fm="pc",key=NULL,flip=TRUE,digits=2,title="Omega",
         sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate="oblimin",
         Phi = NULL, option="equal",lavaan=TRUE)

colMeans(A)
sapply(AM,sd)


psych::alpha(AN, check.keys = TRUE)

All <- as.data.frame(subset(FW, select = c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13, AN1, AN2, AN3, AN4, AN5, AN6, AN7, AN8, AN9, AN10, AN11, AN12, AN13)))
All <- na.omit(All)


V <- as.data.frame(subset(Erhebung, select = c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13)))


N <- (rowMeans(N))

korrelationm <- cor(V,N, method = "pearson")
korrelationm <- cor(subset_cor, method = "spearman")
summary(korrelationm)
corrplot(korrelationm)
korrelationm

kr20(V)
kr20(AN)

split_half(Ball)
split_half(All)


cohen.kappa(ratertab, w=NULL,n.obs=NULL,alpha=.05)


install.packages("DescTools")
library(DescTools)
ratertab <- xtabs (~ AN1 + AN3, data = AN)
ratertab
install.packages("irr")
library(irr)


ratertab <- subset(A, select = c(P, Rater2, Rater3))

KappaM(A[,30:120], method = "Fleiss", conf.level = 0.95)


kappam.fleiss(B, exact = FALSE, detail = FALSE)
kappam.fleiss(A[,75:112])  
