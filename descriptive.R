#deskriptive statistik
library(olsrr)
library(moments)
library(car)
library(psych)



#describe(mydata)
#describeBy(mydata, group = mydata$G)
names(mydata) <- make.names(names(mydata))

describe <- describe(mydata)
#describoBy <- describeBy(mydata, group = mydata$G)
writexl::write_xlsx(describe, "describe.xlsx")


#ausreißer in boxplots
boxplot(G~DS, data =mydata)#, xlab = "Gruppe", ylab = "Geschlecht")


#normalvertielung 
shapiro.test(mydata$FL)
hist(mydata$FL)

#(Bei Normalverteilung skewness nahe 0 und kurtosis nahe 3)
skewness(mydata$FL)
kurtosis(mydata$FL)
agostino.test(mydata$FL)
anscombe.test(mydata$FL)


#levene test auf varianzhomogenität
leveneTest(mydata$DS, mydata$G)



colMeans(mydata)
round(cor(mydata), 2)
item_cors$score <- cor(mydata$score) # Summenscore über die Items
sum(mydata)


#gruppenvergleich
library(psych)
library(lsr)
library(readxl)
#library(knitr)
#library(misty)



t.test(Pair(AV$mydata,BV$mydata),  mydata)


t.test(SC~C, mydata,
       alternative = "two.sided",# "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


cohensD(SC~C, data = mydata)


#wilcox/mannwhitney

wilcox.test(DF~E, data = mydata, exact = FALSE, correct = FALSE, conf.int = FALSE)
z <- qnorm(0.005787/2)
r <- z/sqrt(288)
r


#rank


library(lsr)


groups <- mydata

t.test(groups$SV, groups$SN, paired = TRUE, alternative = "two.sided")
cohensD(groups$SV, groups$SN, method="paired")

t.test(groups$FV, groups$FN, paired = TRUE, alternative = "two.sided")
cohensD(groups$FV, groups$FN, method="paired")

#wilcox signed rank
wilcox.test(group$FV, group$FN, mu = 0, alternative = 'two.sided')
wilcox.test(group$SV, group$SN, mu = 0, alternative = 'two.sided')


#wilcox.test(group$FV,group$FN, paired = TRUE, exact = TRUE, correct = TRUE, conf.int = TRUE)





