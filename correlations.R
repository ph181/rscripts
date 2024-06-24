
library(correlation)
library(corrplot)
library(GGally)
library(gplots)


correlations <- as.data.frame (cov(mydata, method = "pearson"))
pairs(mydata, data=subset_cor, pch = 20, cex=3, col="steelblue",  upper.panel=NULL, cex.labels=1.5)
warnings()


correlations <- cor(mydata, method = "pearson")
correlations <- cor(mydata, method = "spearman")
correlations <- correlation(subset_cor, method = "auto")

correlations <- cor.test(mydata$FV, mydata$FN, 
         alternative = "two.sided", 
         method = "spearman",       
         use = "complete")    


summary(correlations)
corrplot(correlations)

correlations

library(writexl)
x <- as.data.frame(correlations)

write_xlsx(x, "korrflfw.xlsx")

cor(scale(correlations)) %>% kable()
cor(scale(correlations))
ggpairs(correlations)

# correlation plot
options(repr.plot.width=12, repr.plot.height=9)
ggcorr(correlations)


# heatmap
heatmap.2(as.matrix(scale(korrelationm)),scale = "none")
