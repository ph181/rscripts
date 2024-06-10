#############ctree
library(caTools)
library(partykit)



library(devtools)
library(C50)
library(rpart)
library(ipred)
library(randomForest)
library(RWeka)
library(rattle)
library(RColorBrewer)
library(gbm)
library(caret)
library(reprtree)

swe2 <- as.data.frame  (subset(swe, select=c(G, L, N, FL, SP, SC, Fehler, Dauer, Seiten, DS, DS2, DS3)))
shuffle_index <- sample(1:nrow(swe2))
swe2 <- swe2[shuffle_index, ]
swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS3)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, DS3)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(N, G, FL, SP, SC, DS3)))
PD.klasse$DS3<-as.factor(PD.klasse$DS3) 


PD.klasse[1:4]<-round(PD.klasse[1:4],1)
#PD.klasse[1:7]<-round(PD.klasse[1:7],1)



sample_data <- sample.split(PD.klasse, SplitRatio = 0.8)
train <- subset(PD.klasse, sample_data == TRUE)
test <- subset(PD.klasse, sample_data == FALSE)
prop.table(table(train$DS3))
prop.table(table(test$DS3))


tree1<-ctree(DS3~., data=train) #set the model for the tree, predicting class by sepal length , data set being used is iris 
plot(tree1) #view the decision tree


pred = predict(tree1, test[,-5])
cm = confusionMatrix(test$DS3, pred)
print(cm)

