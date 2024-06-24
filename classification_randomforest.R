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


#PD.klasse <- as.data.frame  (subset(swe, select=c(G, L, Fehler, Dauer, Fortschritt, Seiten, FL, SP, SC)))
#PD.klasse <- as.data.frame  (subset(swe, select=c(G, L, Fehler, Dauer, Fortschritt, Seiten)))
PD.klasse <- as.data.frame  ((subset(pmm, select=c(G, FL, SP, SC, Fehler, Dauer, Seiten, DS, DF))))
PD.klasse <- PD.klasse[complete.cases(PD.klasse),]
PD.klasse$DS3<-as.factor(PD.klasse$DS)
#PD.klasse[1:5]<-round(PD.klasse[1:5],1)



#Random Forest is variation on Bagging of decision trees by reducing the attributes available to making a tree at each decision point to a random sub-sample. 
#This further increases the variance of the trees and more trees are required.
#############################https://www.listendata.com/2014/11/random-forest-with-r.html

#find DS
PD.klasse$DSneu <- ifelse(PD.klasse$DS > 0, 1, 0)
PD.klasse$DSneu<-as.factor(PD.klasse$DSneu) 

set.seed(1234)
fit <-randomForest(DSneu~.,data=PD.klasse, ntree=500) 
# If a dependent variable is a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
print(fit)

mtry <- tuneRF(PD.klasse[-6], PD.klasse$DSneu, ntreeTry=500,
               stepFactor=1.5, improve=0.001, trace=TRUE, plot=TRUE)


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



# Laden Sie zuerst das benötigte Paket
install.packages("randomForest")
library(randomForest)

# Angenommen, Ihr Datensatz heißt 'daten' und 'spielErgebnis' ist Ihre Zielvariable
set.seed(123)  # Für reproduzierbare Ergebnisse

# Teilen Sie Ihre Daten in Trainings- und Testsets auf
trainIndex <- sample(1:nrow(PD.klasse), nrow(PD.klasse)*0.7)
trainSet <- PD.klasse[trainIndex,]
testSet <- PD.klasse[-trainIndex,]

# Erstellen Sie das Random Forest Modell
rfModel <- randomForest(DF ~ ., data=trainSet, ntree=500)

# Vorhersagen auf dem Testset machen
predictions <- predict(rfModel, newdata=testSet)

# Modellgenauigkeit berechnen
accuracy <- sum(diag(table(testSet$DF, predictions))) / nrow(testSet)
print(paste('Genauigkeit:', accuracy))


# Laden Sie zuerst das benötigte Paket
install.packages("e1071")
library(e1071)

# Angenommen, Ihr Datensatz heißt 'daten' und 'spielErgebnis' ist Ihre Zielvariable

# Teilen Sie Ihre Daten in Trainings- und Testsets auf
daten <- as.data.frame(scale(PD.klasse))

trainIndex <- sample(1:nrow(daten), nrow(daten)*0.7)
trainSet <- daten[trainIndex,]
testSet <- daten[-trainIndex,]

# Erstellen Sie das SVM Modell
svmModel <- svm(DF ~ ., data=trainSet, kernel="radial")

# Vorhersagen auf dem Testset machen
predictions <- predict(svmModel, newdata=testSet)

# Modellgenauigkeit berechnen
accuracy <- sum(diag(table(testSet$DF, predictions))) / nrow(testSet)
print(paste('Genauigkeit:', accuracy))

