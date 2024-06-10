
install.packages("stargazer")
library(stargazer)
library(jtools)
library(psych)

mydata <- Erhebung
mydata <- mydata[which(mydata$C ==0),]

#Voraussetzung

library(olsrr)
library(jtools)
library(moments)
library(lmtest)


mydata <- Erhebung
mydata <- mydata[which(mydata$C ==1),]

#Aufruf der Regression
# Nur hier sind Anpassungen an Ihre Daten erforderlich
# Regressionsgleichung
reg.fit <- lm(FL ~ C + G + SP + SC + SV + FV, data = mydata)

summ(reg.fit)

# Parameter für den Output der Regression
konfidenz <- 0.95
nachkomma <- 3
# 1 Ausgabe (mit jtools-Package)
# 1.1 Unstandardisierte Ergebnisse
summary(reg.fit, confint=TRUE, ci.width = konfidenz, digits = nachkomma)
# 1.2 Standardisierte Ergebnisse
summary(reg.fit, scale=TRUE, transform.response = TRUE, digits=nachkomma)

# 2 Regressionsdiagnostik (mit olsrr-Package, soweit nicht anders angegeben)
# 2.1 Homoskedastizität
# 2.1.1 Graphischer Test
# (sollte eine chaotische Punktwolke sein; problematisch insbesondere
# ein "liegender Trichter" oder eine erkennbar gebogene Struktur)
ols_plot_resid_fit(reg.fit)

# 2.1.2 Breusch Pagan Test - Signifikanztest auf Heteroskedastizität
# (signifikant => Heteroskedastizität)
ols_test_breusch_pagan(reg.fit)

# 2.2 Normalverteilung der Residuen

# 2.2.1 Histogramm der Residuen
# (Histogramm sollte möglichst nahe an der
# Normalverteilung liegen,
# vor allem an den beiden Außenrändern der Verteilung)
ols_plot_resid_hist(reg.fit)

# 2.2.2 QQ-Plot
# (Punkte sollten möglichst nahe an der Diagonalen liegen)
ols_plot_resid_qq(reg.fit)

# 2.2.3 Shapiro-Wilk-Test auf Normalverteilung
# (signifikant => Residuen nicht normalverteilt)
shapiro.test(reg.fit$residuals)

# 2.2.4 Schiefe und Kurtosis (mit moments-Package)
#(Bei Normalverteilung skewness nahe 0 und kurtosis nahe 3)
skewness(reg.fit$residuals)
kurtosis(reg.fit$residuals)

# 2.2.5 Signifikanztests für Schiefe und Kurtosis
#(mit moments-Package)
#(signifikant = Residuen nicht normalverteilt)
agostino.test(reg.fit$residuals)
anscombe.test(reg.fit$residuals)

# 2.3 Linearität

# 2.3.1 Paarweises Streudiagramm
# (relevant sind nur diejenigen Streudiagramme,
# die die Kriteriumsvariable mit einschließen)
# Daten für die graphische Linearitätsprüfung
attach(mydata)
daten.plot <- data.frame(FL, G, SP, SC, SV, FV)
detach(mydata)
pairs(daten.plot, pch = 19, lower.panel = NULL)

# 2.3.2 Rainbow-Test (mit lmtest-Package) auf Linearität
# (signifikant => Verletzung der Linearität)
raintest(reg.fit)

# 2.4 Abwesenheit von starker Multikollinearität
# (Problematisch: VIF-Werte über 10.0)
ols_vif_tol(reg.fit)

# 2.5 Ausreißerdiagnostik
# 2.5.1 Studentisierte Residuen
# (problematisch: Werte absolut über 3 )
ols_plot_resid_stud(reg.fit)

# 2.5.2 Cook's Distanz
# (unterschiedliche Cut-Off-Werte in der Literatur
# die Voreinstellung hier 4/N ist extrem konservativ,
ols_plot_cooksd_chart(reg.fit)

# 2.5.3 Outlier & Leverage
# (problematisch insbesondere Werte: "outlier & leverage")
ols_plot_resid_lev(reg.fit)

# 2.5.4 DiffBeta
# (Welche Beobachtungen haben einen großen Einfluss auf die
# Parameterschätzung?)
ols_plot_dfbetas(reg.fit)

# 2.6. Unabhängigkeit/Unkorreliertheit der Residuen

# Die Unabhängigkeit der Residuen ergibt sich aus der
# Stichprobenziehung (Querschnittsdesign ohne
# Cluster/hierarchische Datenstrukturen).

# 2.7 Skaleneigenschaften

# Die Eignung der Skaleneigenschaften ergibt sich aus
# der Betrachtung der verwendeten Skalen
# (keine empirische Prüfung). 

# Autokorrelation überprüfen (Durbin-Watson-Test)
car::durbinWatsonTest(reg.fit)
car::vif(reg.fit)


#mydata <- mydata[complete.cases(mydata),]

describeBy(mydata)

# 1 Einfache Regression
fit_e <- lm(FLo ~ G, data = mydata)


#confidenceEllipse(fit_e, levels=0.95, Scheffe=FALSE)
summ(fit_e, digits=3)
summ(fit_e, scale=TRUE, transform.response = TRUE, digits=2)
summary(fit_e)
#stargazer(fit_e, type = "text", digits = 2, out = "regression.txt")
#stargazer(fit_e, type = "html", digits = 2, out = "regression.html")
confint(fit_e, "C", level = 0.95)


# 2 Multiple Regression
fit_m <- lm(FN ~ G*FL +  SP + SC + FV  , data = mydata)#, subset = (G == 0))

summ(fit_m, digits=3)
summ(fit_m, scale=TRUE, transform.response = TRUE, digits=2)
summary(fit_m)

fit_m <- lm(DF ~ FL +  SP + SC + G, data = mydata)#, subset = (G == 0))
summ(fit_m, digits=3)
summ(fit_m, scale=TRUE, transform.response = TRUE, digits=2)
summary(fit_m)



stargazer(fit_m, type = "html", digits = 2, out = "regression.html")

stargazer(fit_m, type = "text", digits = 2, out = "regression.txt")

# Vergleich der Koeffizienten

modell <- lm(scale(FL)~scale(SP)+scale(SC)+scale(FV), data = mydata, subset = (G == 0))#, subset = (G == 1))
summ(modell)


# 3 Datensätze selektieren
fit_w <- lm(SN ~ SV+FL, data = mydata, subset = (G == 0))
summ(fit_w, digits=3)

fit_m <- lm(FL ~  SP + SC + SV + FV, data = mydata, subset = (G == 1))
summ(fit_m, digits=3)


fit_n <- lm(FL ~ FV+ SV + SP + SC , data = mydata, subset = c(FL <= 3.45))
summ(fit_n, digits=3)

fit_n <- lm(FL ~ FV+ SV + SP + SC + N, data = mydata, subset = (N >= 0.5))
summ(fit_n, digits=3)



# 4 Hierarchische Regression

# 4b Zwei Regressionen

fit_1 <- lm(FL ~ FV + SP+  N, data = mydata)
fit_2 <- lm(FL ~ FV+ SV + SP + SC + N, data = mydata)

summ(fit_1, digits=3)
summ(fit_2, digits=3)

# Test für den zweiten Regressionsschritt
anova(fit_1, fit_2)

# Delta R²
summary(fit_2)$r.squared - summary(fit_1)$r.squared
# Regression mit Bootstraping
# Initialisierung Zufallszahlengenerator
set.seed(12345)

# Eigentliches Bootstrapping
model_b <- lm(data = mydata, FL ~ FV + SP)
fit_b <- Boot(model_b, R = 5000)

#Ergebnisse
summary(fit_b)
confint(fit_b, level = .95)

# Histogramm für die Parameterschätzung
hist(fit_b) 
summ(model_b)



#Moderation
library(rockchalk)
library(interactions)
library(jtools)
library(psych)

mydata <- Erhebung
mydata <- Erhebung[which(Erhebung$C ==0),]


#moderation
process(data = mydata, y = "FN", x = "FV", w ="Flo"
        , cov = c( "SC", "SP", "G", "SV")
        , model = 1, center = 1, moments = 1, jn = 1, modelbt = 1, boot = 10000, seed = 654321)


process (data = subset(mydata, G==1), y = "SN", x = "SV", w ="FL"
         #     , cov = c("SP")
         , model = 1, center = 1) 

#model 2 doppelmoderation
process(data = mydata, y = "DFW", x = "FL", w ="SP", z = "SC"
        #        , cov = c("G")
        , model = 2, center = 2, moments = 1, jn = 1, modelbt = 1, boot = 10000, seed = 654321) #!!!

#model 3 moderierte moderation
process(data = mydata, y = "DFW", x = "SP", w ="SC", z = "FL", cov = c("G"), model = 3, center = 2, moments = 1, jn = 1, modelbt = 1, boot = 10000, seed = 654321) #!!!



interactionModel <- lm(FN  ~ Flo*FV + G + SP +SC + SV, data = mydata)
summ(interactionModel)
interact_plot(interactionModel, pred = "FV", modx = "Flo")
interactions::johnson_neyman(interactionModel, pred = "FV", modx = "Flo", alpha = 0.05)
plotSlopes (interactionModel, plotx ="FV" , modx = "Flo", modxVals = "std.dev." ) 




mydata <- Erhebung
mydata <- Erhebung[which(Erhebung$C ==0),]


#Mediation

#Installieren des bda Pakets falls nicht bereits installiert
install.packages('bda')
#Laden des bda Pakets
library(bda)

mv <- FLo$mydata
iv <- SV$mydata
dv <- SN$mydata

mediation.test(mv,iv,dv)


#Mediation mit KOntrolle
process (data = mydata, y = "SN", x = "SV", m ="FLo"
         , cov = c( "SC", "SP", "G", "FV")
         ,model = 4, effsize =1, total =1,stand =1, modelbt = 1, seed = 654321) #mediation

process (data = mydata, y = "FN", x = "FV", m ="FL"
         #, cov = "G"
         , model = 4, effsize =1, total =1,stand =1, modelbt = 1, seed = 654321) #mediation


process (data = mydata, y = "FN", x = "FV", m ="FL"
         #   , cov = "G"
         , model = 4, effsize =1, total =1,stand =1, modelbt = 1, seed = 654321) #mediation

#moderierter c pfad
process (data = 
           #  subset(mydata, G==0),
           mydata,
         y = "FN", x = "FV", m ="FL", w="SP", model = 5, effsize =1, total =1,stand =1, boot = 10000 , modelbt = 1, seed = 654321) #nix

#moderierter a pfad
process (data = mydata, y = "FN", x = "FV", m ="FL",w="SP", model = 7, effsize =1, total =1,stand =1, modelbt = 1, seed = 654321) #bisschenS

#moderierter b pfad
process (data = mydata, y = "FN", x = "FV", m ="FL",w="SP", model = 14, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#

#mediation mit Parallel
process (data = mydata, y = "DFV", x = "FL", m =c("SP","SC"), model = 4, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!

#serielle mediation
process (data = mydata, y = "FN", x = "FV", m =c("FL","SP"), model = 6, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!

#moderierter a und c pfad
process (data = mydata, y = "FN", x = "FV", m= "FL", w ="SP", model = 8, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!

# 14
#model 15 moderation von c' und b
process (data = subset(PD, G==0), y = "FN", x = "FV", m= "FL", w ="SP", model = 15, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!

#model 58 moderation auf a und b
process (data = PD, y = "FN", x = "FV",m= "FL",w ="SP",    model = 58, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!

#moderation 59 aif a b und c'
process (data = subset(PD, G==0), y = "FN", x = "FV",m= "FL",w ="N", model = 59, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)#!!!





#regression stepwise

PD <- as.data.frame  (subset(Erhebung, select=c(FL, SC, SP, G, L, N, DF, DS, FN, FV, SN, SV)))
PD <- PD[complete.cases(PD),]

PD <- swe3

Modell <- lm(FL ~  FV + SP + SC, data = mydata)#subset(PD, G==0))

Modell <- lm(SN ~  FV + SV + FL + SP + SC + G + N, data = PD)

Modell <- lm(FL ~  FV + SV + SP + SC + G + N, data = PD)


# Signifikante Variablen werden dem Modell schrittweise hinzugefügt
MASS::stepAIC(Modell, direction = "forward")
#http://127.0.0.1:10729/graphics/plot_zoom_png?width=1920&height=1017
# Nicht-signifikante Variablen werden aus dem Modell schrittweise entfernt
MASS::stepAIC(Modell, direction = "backward")

# Variablen werden dem Modell schrittweise hinzugefügt und entfernt
MASS::stepAIC(Modell, direction = "both")


leaps <- leaps::regsubsets(FL ~  FV + SP + SC, nbest=5, data = PD)#, subset = (G == 1))


#https://communities.sas.com/t5/Statistical-Procedures/What-is-the-cutoff-value-for-Information-Criteria-like-http://127.0.0.1:37227/graphics/plot_zoom_png?width=1200&height=900AIC-and/m-p/160059#M8334
# Zusammenfassung der 5 besten Modelle anzeigen
summary(leaps)

# Die 5 besten Modelle grafisch darstellen (mit R²)
plot(leaps)



# Relative Wichtigkeit eines Modells berechnen
relaimpo::calc.relimp(Modell, type = c("lmg", "last", "first", "betasq", "pratt"), rela = TRUE )

# Regression mit Bootstrapping und relativer Wichtigkeit berechnen
bootstrapRegression <- relaimpo::boot.relimp(Modell, type = c("lmg", "last", "first", "betasq", "pratt"), rela = TRUE, b = 5000, diff = T, rank = T )
relaimpo::booteval.relimp(bootstrapRegression)

# Grafische Ausgabe der Berechnungen
plot(relaimpo::booteval.relimp(bootstrapRegression, sort=TRUE))


#nonlinear


library(drc)
install.packages("gam")

library(gam)
a <- lm(DF ~ FL+SP  + N, data = swe)
summary(a)

library(interactions)

a <- lm(DF ~ FL*SP, data = swe)
summary(a)

process (data = PD, y = "DF", x = "SP", w ="FL",  model = 1, center = 1) 

interactionModel <- lm(DF  ~ SP+FL, data = swe)
summary(interactionModel)



interact_plot(interactionModel, pred = "SP", modx = "FL")
interactions::johnson_neyman(interactionModel, pred = "SP", modx = "FL", alpha = 0.05)

glm(
  DF ~ FL*(1/SP),
  family = gaussian,
  data = swe,
  #  weights,
  # subset,
  #na.action,
  #  start = NULL,
  # etastart,
  #  mustart,
  # control = gam.control(...),
  #  model = TRUE,
  #  method = "glm.fit",
  #  x = FALSE,
  #  y = TRUE,
)



nls(DF ~ FL*(1/SP), data = swe
    #,start, control, algorithm, trace, subset, weights, na.action, model, lower, upper,
)
