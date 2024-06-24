library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(C50)
library(RWeka)
library(caret)

###############################

mydata$kategorie<-as.factor(mydata$kategorie) 

set.seed(1211)


#use 70% of dataset as training set and 30% as test set
#sample <- sample(c(TRUE, FALSE), nrow(PD.klasse), replace=TRUE, prob=c(0.7,0.3))
#train  <- PD.klasse[sample, ]
#test   <- PD.klasse[!sample, ]

split <- sample.split(mydata, SplitRatio = 0.7)
train <- subset(mydata, split == "TRUE")
test <- subset(mydata, split == "FALSE")

prop.table(table(train$kategorie))
prop.table(table(test$kategorie))


  ###############
#C4.5 The C4.5 algorithm is an extension of the ID3 algorithm and constructs a decision tree to maximize information gain (difference in entropy).

fit <- J48(kategorie~., data=train, 
           #control = Weka_control(R = TRUE)
           )

# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, test)

# summarize accuracy
if(require("partykit", quietly = TRUE)) plot(fit)
summary(predictions)
table_mat <- table(predictions, test$kategorie)
table_mat <- table(test$kategorie, predictions)

summary(table_mat)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


eval_j48 <- evaluate_Weka_classifier(fit, numFolds = 10, complexity = TRUE, 
                                     seed = 1, class = TRUE)
eval_j48



#######################
#Boosted C5.0 The C5.0 method is a further extension of C4.5 and pinnacle of that line of methods. 
#It was proprietary for a long time, although the code was released recently and is available in the C50 package.
set.seed(1211)

split <- sample.split(mydata, SplitRatio = 0.7)
train <- subset(mydata, split == "TRUE")
test <- subset(mydata, split == "FALSE")
prop.table(table(train$kategorie))
prop.table(table(test$kategorie))

fit <- C5.0(kategorie~., data=train)
# summarize the fit
summary(fit)
print(fit)
# make predictions
predictions <- predict.C5.0(fit, test, type="class")
# summarize accuracy
table_mat <- table(predictions, test$kategorie)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
if(require("partykit", quietly = TRUE)) plot(fit)

summary(predictions)


rules <- C5.0(mydata[,-7], mydata[,7], rules = TRUE)
summary(rules) # view the ruleset  





# PART PART is a rule system that creates pruned C4.5 decision trees for the data set and extracts rules and those instances that are covered by 
#the rules are removed from the training data. The process is repeated until all instances are covered by extracted rules.
fit <- PART(kategorie~., data=train)
# summarize the fit
summary(fit)

# make predictions
predictions <- predict(fit, test)
# summarize accuracy
table_mat <- table(predictions, test$kategorie)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


############
set.seed(1958)  # set a seed to get replicable results
trains <- createFolds(mydata$kategorie, k=10)
C45Fit <- train(kategorie ~., method="J48", data=mydata,
                tuneLength = 5,
                trControl = trainControl(
                  method="cv", indexOut=trains))
C45Fit


