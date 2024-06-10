library(rpart)
library(rpart.plot)
library(caTools)




swe <- as.data.frame  (subset(Erhebung, select=c(C, G, SP, SC, SV, DFW)))
shuffle_index <- sample(1:nrow(swe))
swe <- swe[shuffle_index, ]
swe <- swe[complete.cases(swe),]
#swe$FL3<-as.factor(swe$FL3) 
swe$DFW<-as.factor(swe$DFW) 
swe[1:5]<-round(swe[1:5],1)


sample_data <- sample.split(swe, SplitRatio = 0.8)
train <- subset(swe, sample_data == TRUE)
test <- subset(swe, sample_data == FALSE)
prop.table(table(train$DFW))
prop.table(table(test$DFW))

#When rpart grows a tree it performs 10-fold cross validation on the data. Use printcp() to see the cross validation results.

fit <- rpart(DFW~., data = train, 
             method = 'class',
             parms = list(split = 'DSC3'), 
             maxdepth = 4, 
             minsplit = 2, 
             minbucket = 1,
             cp = -1)

rpart.plot(fit, 
           #tweak = 1, 
           cex=0.5, 
           fallen.leaves = FALSE,
           compress=FALSE,
           ycompress=FALSE,
           snip = TRUE,
           uniform =TRUE
)
fancyRpartPlot(fit, caption = NULL)

printcp(fit)

plotcp(fit) 

rsq.rpart(fit)

print(fit) 	#print results
summary(fit) 	#detailed results including surrogate splits
plot(fit) #	plot decision tree
text(fit) 	#label the decision tree plot
post(fit, data = swe$DFW)

prune(fit, cp= 0.1) 

fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]




predict_unseen <-predict(fit, 
                         test, 
                         type = 'class')



table_mat <- table(test$DFW, 
                   predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, 
                            test, 
                            type = 'class')
  table_mat <- table(test$DFW, 
                     predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  summary(accuracy_Test)
}
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 5,
                         cp = 0,
                         xval=10
)
tune_fit <- rpart(DFW~.,                                           
                  data = train, 
                  method = 'class', 
                  control = control)


accuracy_tune(tune_fit)
fancyRpartPlot(tune_fit, caption = NULL)


rpart.plot(fit, 
           extra = 100)


fit$variable.importance
printcp(fit)
fit <- prune(fit, cp = -1)
fancyRpartPlot(fit, caption = NULL)



predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$DFW, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test, type = 'class')
  table_mat <- table(test$DFW, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(3),
                         maxdepth = 3,
                         cp = -1)
tune_fit <- rpart(train$DFW~., 
                  data = train, 
                  method = 'class', 
                  control = control)


accuracy_tune(tune_fit)

rpart.plot(tune_fit)



#regression tree
fit2 <- rpart(DFW~.,
              data = train, 
              method="anova") #method=anova für regression tree
plot(fit2, compress =TRUE)
text(fit2,pretty=3)
summary(fit2)
fancyRpartPlot(fit2, caption = NULL)


fit2$variable.importance
printcp(fit2)
fit2 <- prune(fit, cp = -1)
fancyRpartPlot(fit2, caption = NULL)

rpart.plot(fit2)


#CART Classification and Regression Trees (CART) split attributes based on values that minimize a loss function, such as sum of squared errors.
fit <- rpart(DFW~., data=train)
# summarize the fit
summary(fit)
rpart.plot(fit, 
           extra = 100)
# make predictions

predictions <- predict(fit, test, type="class")
# summarize accuracy
table_mat <- table(predictions, test$DFW)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

fancyRpartPlot(fit, caption = NULL)

output.tree <- rpart(DFW~.,data = train, method="class")
plot(output.tree)
text(output.tree,pretty=100)
summary(output.tree)
rpart.plot(output.tree)
fancyRpartPlot(output.tree)

prediction_model <- predict(output.tree,test,type="vector")


MAE <- function(actual,pred) {mean(abs(actual-pred))}
MAE(test$DSc,prediction_model)
MSE1 <- mean((prediction_model-test$DFW)^2)

printcp(output.tree)
plotcp(output.tree)


pruned_model <- prune.rpart(output.tree,cp=0.01)
plot(pruned_model)

text(pruned_model)

fancyRpartPlot(pruned_model)


y1 <- predict(pruned_model, test)

plot(y1, test$DFW)

abline(0,1)

MSE2 <- mean((y1-test$DFW)^2)

MSE2

