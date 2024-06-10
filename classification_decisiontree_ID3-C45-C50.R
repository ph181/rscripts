library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(C50)
library(RWeka)

install.packages("RWeka")

###############################
swe <- as.data.frame  (subset(Erhebung, select=c(C, G, SP, SC, SV, DFW)))
shuffle_index <- sample(1:nrow(swe))
swe <- swe[shuffle_index, ]
swe <- swe[complete.cases(swe),]
#swe$FL3<-as.factor(swe$FL3) 
swe$DFW<-as.factor(swe$DFW) 
swe[1:5]<-round(swe[1:5],1)


set.seed(1211)


#use 70% of dataset as training set and 30% as test set
#sample <- sample(c(TRUE, FALSE), nrow(PD.klasse), replace=TRUE, prob=c(0.7,0.3))
#train  <- PD.klasse[sample, ]
#test   <- PD.klasse[!sample, ]

split <- sample.split(swe, SplitRatio = 0.7)
train <- subset(swe, split == "TRUE")
test <- subset(swe, split == "FALSE")

prop.table(table(train$DFW))
prop.table(table(test$DFW))


  ###############
#C4.5 The C4.5 algorithm is an extension of the ID3 algorithm and constructs a decision tree to maximize information gain (difference in entropy).

fit <- J48(DFW~., data=train, 
           #control = Weka_control(R = TRUE)
           )

# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, test)

# summarize accuracy
if(require("partykit", quietly = TRUE)) plot(fit)
summary(predictions)
table_mat <- table(predictions, test$DFW)
table_mat <- table(test$DFW, predictions)

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

split <- sample.split(swe, SplitRatio = 0.7)
train <- subset(swe, split == "TRUE")
test <- subset(swe, split == "FALSE")
prop.table(table(train$DFW))
prop.table(table(test$DFW))

fit <- C5.0(DFW~., data=train)
# summarize the fit
summary(fit)
print(fit)
# make predictions
predictions <- predict.C5.0(fit, test, type="class")
# summarize accuracy
table_mat <- table(predictions, test$FLW)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
if(require("partykit", quietly = TRUE)) plot(fit)

summary(predictions)


rules <- C5.0(swe[,-7], swe[,7], rules = TRUE)
summary(rules) # view the ruleset  





# PART PART is a rule system that creates pruned C4.5 decision trees for the data set and extracts rules and those instances that are covered by 
#the rules are removed from the training data. The process is repeated until all instances are covered by extracted rules.
fit <- PART(DFW~., data=train)
# summarize the fit
summary(fit)

# make predictions
predictions <- predict(fit, test)
# summarize accuracy
table_mat <- table(predictions, test$DFW)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


############
library(caret)
set.seed(1958)  # set a seed to get replicable results
trains <- createFolds(swe$DFW, k=10)
C45Fit <- train(DFW ~., method="J48", data=swe,
                tuneLength = 5,
                trControl = trainControl(
                  method="cv", indexOut=trains))
C45Fit


