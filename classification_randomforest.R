# Installing package
install.packages("caTools")	 # For sampling the dataset
install.packages("randomForest") # For implementing random forest algorithm
install.packages("caret")
install.packages("mlbench")
install.packages("rfviz")
install.packages("ROCR")
install.packages("randomForestExplainer")

# Loading package
library(caTools)
library(randomForest)
library(mlbench)
library(caret)
library(ROCR)
library(randomForestExplainer )
library(rfviz )

swe2 <- as.data.frame  (subset(swe, select=c(G, L, FL, SP, SC, Fehler, Dauer, Fortschritt, Seiten, DS, DS2, DS3)))
shuffle_index <- sample(1:nrow(swe2))
swe2 <- swe2[shuffle_index, ]
swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe, select=c(G, L, Fehler, Dauer, Fortschritt, Seiten, FL, SP, SC)))
#PD.klasse <- as.data.frame  (subset(swe, select=c(G, L, Fehler, Dauer, Fortschritt, Seiten)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, L, FL, SP, SC, DS)))
PD.klasse$DS3<-as.factor(PD.klasse$DS)
PD.klasse[1:5]<-round(PD.klasse[1:5],1)



#Random Forest is variation on Bagging of decision trees by reducing the attributes available to making a tree at each decision point to a random sub-sample. 
#This further increases the variance of the trees and more trees are required.
#############################https://www.listendata.com/2014/11/random-forest-with-r.html

#find DS
PD.klasse$DSneu <- ifelse(swe$DS > 0, 1, 0)
PD.klasse$DSneu<-as.factor(PD.klasse$DSneu) 

set.seed(1234)
fit <-randomForest(DSneu~.,data=PD.klasse, ntree=500) 
# If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
print(fit)

mtry <- tuneRF(PD.klasse[-6],PD.klasse$DSneu, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(1234)
fit <-randomForest(DSneu~.,data=PD.klasse, mtry=best.m, importance=TRUE,ntree=500)
print(fit)
#Evaluate variable importance
importance(fit)
varImpPlot(fit)
#Higher the value of mean decrease accuracy or mean decrease gini score , 
#higher the importance of the variable in the model.

pred1=predict(fit,type = "prob")
perf = prediction(pred1[,2], PD.klasse$DSneu)
# 1. Area under curve
auc = performance(perf, "auc")

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


reprtree:::plot.getTree(fit)
explain_forest(fit, interactions = TRUE, data = PD.klasse)

##################regression

rf <-randomForest(DSneu~.,data=PD.klasse, ntree=500) 
# If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
print(rf)

mtry <- tuneRF(PD.numklas[-7],PD.klasse$DSneu, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(1234)
rf <-randomForest(DSneu~.,data=PD.klasse, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)
#Higher the value of mean decrease accuracy or mean decrease gini score , 
#higher the importance of the variable in the model.

pred1=predict(rf,type = "prob")
perf = prediction(pred1[,2], PD.klasse$DSneu)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



plot(rf)
reprtree:::plot.getTree(rf)




library("party")
cf <- cforest(DS~., data=PD.klasse)

pt <- prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 

plot(nt, type="simple")


getTree(fit)

