library(psych)
library(lsr)
library(readxl)
#library(knitr)
#library(misty)




alle_voll <- Erhebung
alle_voll <- Erhebung[which(Erhebung$ C==0),]
alle_voll <-  as.data.frame(subset(Erhebung, select = c(C, SC)))


describeBy(alle_voll,alle_voll$C)

describe(alle_voll)

t.test(Pair(AV$alle_voll,BV$alle_voll),  alle_voll)


t.test(SC~C, alle_voll,
       alternative = "two.sided",# "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


cohensD(SC~C, data = alle_voll)


#wilcox/mannwhitney

wilcox.test(DF~E, data = alle_voll, exact = FALSE, correct = FALSE, conf.int = FALSE)
z <- qnorm(0.005787/2)
r <- z/sqrt(288)
r
