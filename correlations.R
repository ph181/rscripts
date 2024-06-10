
library(correlation)
library(corrplot)
library(GGally)
library(gplots)

subset_cor <- as.data.frame(subset(Erhebung, select=c(AV1, AV2, AV3, AV4, AV5, AV6, AV7, AV8, AV9, AV10, AV11, AV12, AV13, BN1, BN2, BN3, BN4, BN5, BN6, BN7, BN8, BN9, BN10, BN11, BN12, BN13)))
arg <- Erhebung[which(Erhebung$L ==0),]
subset_cor <- subset_cor[complete.cases(subset_cor),]

b <- as.data.frame (cov(subset_cor, method = "pearson"))
pairs(subset_cor, data=subset_cor, pch = 20, cex=3, col="steelblue",  upper.panel=NULL, cex.labels=1.5)
warnings()


korrelationm <- cor(subset_cor, method = "pearson")
korrelationm <- cor(subset_cor, method = "spearman")
summary(korrelationm)
corrplot(korrelationm)

korrelationm

library(writexl)
x <- as.data.frame(korrelationm)

write_xlsx(x, "korrflfw.xlsx")

cor(scale(korrelationm)) %>% kable()
cor(scale(korrelationm))
ggpairs(korrelationm)

# correlation plot
options(repr.plot.width=12, repr.plot.height=9)
ggcorr(subset_cor)


# heatmap
heatmap.2(as.matrix(scale(korrelationm)),scale = "none")



correlation(subset_cor, method = "auto")
    

correlation(subset_cor, method = "pearson")



cor.test(subset_cor$FV, subset_cor$FN, 
         alternative = "two.sided", 
         method = "spearman",       
         use = "complete")                    


AN$score <- rowSums(AN)
a <- round(cor(AN), 2)
a <- as.data.frame(AN)

write_xlsx(a, "a.xlsx")


BN$score <- rowSums(BN)
b <- round(cor(BN), 2)
b <- as.data.frame(BN)
write_xlsx(b, "b.xlsx")

cor.test(AN$AN1,AN$score)
cor.test(AN$AN2,AN$score)
cor.test(AN$AN3,AN$score)
cor.test(AN$AN4,AN$score)
cor.test(AN$AN5,AN$score)
cor.test(AN$AN6,AN$score)
cor.test(AN$AN7,AN$score)
cor.test(AN$AN8,AN$score)
cor.test(AN$AN9,AN$score)
cor.test(AN$AN10,AN$score)
cor.test(AN$AN11,AN$score)
cor.test(AN$AN12,AN$score)
cor.test(AN$AN13,AN$score)