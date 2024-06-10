#deskriptive statistik
library(olsrr)#
library(moments)
library(car)
library(psych)


mydata <- Erhebung
mydata <- mydata[which(mydata$C ==0),]

model <- lm(FL ~ SV + SP + SC +FV, data = mydata)

vif(model)
1/vif(model)

summary(model)


describe(Erhebung)

alle_voll <- as.data.frame(subset(Erhebung, select=c(C, S, L, G, FL, SP, SC, FV, FN, SV, SN)))
alle_voll <- alle_voll[complete.cases(alle_voll),]

alle_voll <- Erhebung[which(Erhebung$C ==1),]
alle_voll <- as.data.frame(subset(alle_voll[,paste0("SC",1:7)]))
describo <- describe(alle_voll)
describo
writexl::write_xlsx(describo, "describo.xlsx")

describo <- describeBy(alle_voll, group = alle_voll$C)

describo <- describe(mydata)

alle_voll <- as.data.frame(subset(Erhebung, select=c(AN1, AN2, AN3, AN4, AN5, AN6, AN7, AN8, AN9, AN10, AN11, AN12, AN13)))

AN <- AN[complete.cases(AN),]



#ausreißer in boxplots
boxplot(D~G, data =alle_voll)#, xlab = "Gruppe", ylab = "Geschlecht")


#normalvertielung 

shapiro.test(alle_voll$AN13)

shapiro.test(alle_voll$FL)


hist(alle_voll$DS)



#(Bei Normalverteilung skewness nahe 0 und kurtosis nahe 3)
skewness(alle_voll$DS)
kurtosis(alle_voll$DS)
agostino.test(alle_voll$DS)
anscombe.test(alle_voll$DS)


#levene test auf varianzhomogenität
leveneTest(alle_voll$N, alle_voll$C)



colMeans(AN)
round(cor(AN), 2)
item_cors$score <- cor(AN$score) # 6. Spalte = Summenscore über die Items


sum(AN)
