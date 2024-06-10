#In association rule mining, Support, Confidence, and Lift measure association.
#Support says how popular an item is, as measured in the proportion of transactions in which an item set appears.
#Confidence says how likely item Y is purchased when item X is purchased, expressed as {X -> Y}.
#Thus it is measured by the proportion of transaction with item X in which item Y also appears. Confidence might misrepresent the importance of association.
#Lift says how likely item Y is purchased when item X is purchased while controlling for how popular item Y is.

install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(ggplot2)

prozessdaten <- as.data.frame  (subset(Erhebung, select=c(V1, V2.)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.klasse <- prozessdaten[complete.cases(prozessdaten),]

PD.num <- prozessdaten[complete.cases(prozessdaten),]
PD.num<-round(PD.num,2)

PD.numklas <- prozessdaten[complete.cases(prozessdaten),]


PD.klasse$class <-as.factor(PD.num$V1)

trans <- transactions(PD.klasse)

as(trans,"transactions")
summary(trans)
colnames(trans)
image(trans)
itemFrequencyPlot(trans,topN = 20)
vertical <- as(trans, "tidLists")
as(vertical, "matrix")[1:10, 1:6]

set.seed = 220 # Setting seed
rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.5))
summary(rules)
inspect(head(rules, by = "confidence", n= 10))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# Visualising the results
plot(rules,jitter = 1)
plot(rules, 
     shading = "order")
plot(rules,
     n = 12,
     method = "graph", 
     measure = "confidence", 
     shading = "lift",
     control=list(type="items")
)

plot(rules, 
     method="paracoord", 
     control=list(reorder=TRUE))
summary(trans)

SN_rules <- apriori(data=trans, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="SNc=1"))

plot(SN_rules,
     n = 10,
     method = "graph", 
     measure = "confidence", 
     shading = "lift",
     control=list(type="items")
)

plot(SN_rules, 
     method="paracoord", 
     control=list(reorder=TRUE))



save.image("~/R/Rskripte/trunk/association.RData")
