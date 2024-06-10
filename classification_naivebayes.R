<<<<<<< .mine
# Loading package
library(e1071)
library(caTools)
library(caret)
library(klaR)

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(rsample)  # data splitting 

swe2 <- as.data.frame  (subset(swe, select=c(G,FL, SP, SC, Fehler, Dauer, Seiten, DS, DS2, DS3)))
shuffle_index <- sample(1:nrow(swe2))
swe2 <- swe2[shuffle_index, ]
swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS3)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, DS3)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, FL, SP, SC, DS3)))
PD.klasse$DS3<-as.factor(PD.klasse$DS3) 
PD.klasse[1:4]<-round(PD.klasse[1:4],1)
#PD.klasse[1:7]<-round(PD.klasse[1:7],1)



sample_data <- sample.split(PD.klasse, SplitRatio = 0.8)
train <- subset(PD.klasse, sample_data == TRUE)
test <- subset(PD.klasse, sample_data == FALSE)
prop.table(table(train$DS3))
prop.table(table(test$DS3))


# Feature Scaling
#train_scale <- scale(train[, 1:6])
#test_scale <- scale(test[, 1:6])


# Fitting Naive DSC3 Model to training dataset
fit <- naiveBayes(DS3 ~ ., data = train)
fit
# Predicting on test data'
predictions <- predict(fit, newdata = test)
# Confusion Matrix
cm <- table(test$DS3, predictions)
cm
# Model Evaluation
confusionMatrix(cm)


###########

fit <- naive_bayes(DS3 ~ ., data = train) 
fit
plot(fit) 

predictions <- predict(fit, train, type = 'prob')
head(cbind(predictions, train))


p1 <- predict(fit, train)
(tab1 <- table(p1, train$DS3))
1 - sum(diag(tab1)) / sum(tab1)

p2 <- predict(fit, test)
(tab2 <- table(p2, test$DS3))
1 - sum(diag(tab2)) / sum(tab2)

#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(PD.klasse)), PD.klasse$DS3) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
PD.sub_dataset=PD.klasse[repeating_sequence,]
#We no longer need the frequency, drop the feature
PD.sub_dataset$DS3=NULL




#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(DS3 ~., data=train)
#What does the model say? Print the model summary
Naive_Bayes_Model


#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,test)
#Confusion matrix to check accuracy
table(NB_Predictions,test$DS3)



train %>%
  filter(DS3 == "zu") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "kein") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "ab") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>% 
  select(FL, SP, SC, G) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

features <- setdiff(names(train), "DS3")
x <- train[, features]
y <- train$DS3


# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

confusionMatrix(nb.m1)


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

plot(nb.m2)
confusionMatrix(nb.m2)
pred <- predict(nb.m2, newdata = test)



#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, DS)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, FL, SP, SC, DS)))
PD.klasse[1:4]<-round(PD.klasse[1:5],1)
#PD.klasse[1:7]<-round(PD.klasse[1:8],1)


#PD.num$DSC3<-as.factor(PD.num$DSC3) 
set.seed(1234)
# Splitting data into train and test data
split <- sample.split(PD.klasse, SplitRatio = 0.7)
train <- subset(PD.klasse, split == "TRUE")
test <- subset(PD.klasse, split == "FALSE")


#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit a regression model and use k-fold CV to evaluate performance
model <-caret:: train(DS ~ ., data = PD.klasse, method = "lm", trControl = ctrl)

#view summary of k-fold CV               
print(model)

# RMSE: The root mean squared error. This measures the average difference between the predictions made by the model 
# and the actual observations. The lower the RMSE, the more closely a model can predict the actual observations.
# Rsquared: This is a measure of the correlation between the predictions made by the model and the actual observations. 
# The higher the R-squared, the more closely a model can predict the actual observations.
# MAE: The mean absolute error. This is the average absolute difference between the predictions made by the model and 
# the actual observations. The lower the MAE, the more closely a model can predict the actual observations.

model$finalModel
model$resample
sd(model$resample$Rsquared)
###################################################################################
#############################missing values ersetzen
library(mice)
PD.klasse <- as.data.frame(subset(swe, select=c(G, N, L, FL, SP, SC,DSC3)))
PD.klasse$DSC3<-as.factor(PD.klasse$DSC3) 

mice_mod <- mice(PD.klasse, method='rf')
mice_complete <- complete(mice_mod)
PD.klasse$G <- mice_complete$G
PD.klasse$L <- mice_complete$L
PD.klasse$FL <- mice_complete$FL
PD.klasse$SP<- mice_complete$SP
PD.klasse$SC <- mice_complete$SC
set.seed(1234)
PD.klasse <- PD.klasse[complete.cases(PD.klasse),]


indxTrain <- createDataPartition(y = PD.klasse$DSC3,p = 0.75,list = FALSE)
training <- PD.klasse[indxTrain,]
testing <- PD.klasse[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100


#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-6]
y = training$DSC3

fit = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
fit
prediction <- predict(fit,newdata = testing )
confusionMatrix(prediction, testing$DSC3 )
plot(fit)

||||||| .r65
# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("mlr")
install.packages("naivebayes")
install.packages("rsample")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(klaR)

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(rsample)  # data splitting 

we2 <- as.data.frame  (subset(swe, select=c(G, L, N, FL, SP, SC, Fehler, Dauer, Seiten, DS, DS2, DS3)))
shuffle_index <- sample(1:nrow(swe2))
swe2 <- swe2[shuffle_index, ]
swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS3)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, DS3)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, FL, SP, SC, DS3)))
PD.klasse$DS3<-as.factor(PD.klasse$DS3) 
PD.klasse[1:4]<-round(PD.klasse[1:4],1)
#PD.klasse[1:7]<-round(PD.klasse[1:7],1)



sample_data <- sample.split(PD.klasse, SplitRatio = 0.8)
train <- subset(PD.klasse, sample_data == TRUE)
test <- subset(PD.klasse, sample_data == FALSE)
prop.table(table(train$DS3))
prop.table(table(test$DS3))


# Feature Scaling
#train_scale <- scale(train[, 1:6])
#test_scale <- scale(test[, 1:6])


# Fitting Naive DSC3 Model to training dataset
fit <- naiveBayes(DS3 ~ ., data = train)
fit
# Predicting on test data'
predictions <- predict(fit, newdata = test)
# Confusion Matrix
cm <- table(test$DS3, predictions)
cm
# Model Evaluation
confusionMatrix(cm)


table_mat <- table(predictions, test$DS3)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

###########

fit <- naive_bayes(DS3 ~ ., data = train) 
fit
plot(fit) 

predictions <- predict(fit, train, type = 'prob')
head(cbind(predictions, train))


p1 <- predict(fit, train)
(tab1 <- table(p1, train$DSC3))
1 - sum(diag(tab1)) / sum(tab1)

p2 <- predict(fit, test)
(tab2 <- table(p2, test$DS3))
1 - sum(diag(tab2)) / sum(tab2)

#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(PD.klasse)), PD.klasse$DS3) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
PD.sub_dataset=PD.klasse[repeating_sequence,]
#We no longer need the frequency, drop the feature
PD.sub_dataset$DS3=NULL




#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(DS3 ~., data=train)
#What does the model say? Print the model summary
Naive_Bayes_Model


#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,test)
#Confusion matrix to check accuracy
table(NB_Predictions,test$DS3)



train %>%
  filter(DS3 == "zu") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "kein") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "ab") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>% 
  select(FL, SP, SC, G, L) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

features <- setdiff(names(train), "DSC3")
x <- train[, features]
y <- train$DS3


# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

confusionMatrix(nb.m1)


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

plot(nb.m2)
confusionMatrix(nb.m2)
pred <- predict(nb.m2, newdata = test)


#PD.num$DSC3<-as.factor(PD.num$DSC3) 
set.seed(1234)
# Splitting data into train and test data
split <- sample.split(PD.num, SplitRatio = 0.7)
train <- subset(PD.num, split == "TRUE")
test <- subset(PD.num, split == "FALSE")


#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit a regression model and use k-fold CV to evaluate performance
model <-caret:: train(DS ~ ., data = PD.num, method = "lm", trControl = ctrl)

#view summary of k-fold CV               
print(model)

# RMSE: The root mean squared error. This measures the average difference between the predictions made by the model 
# and the actual observations. The lower the RMSE, the more closely a model can predict the actual observations.
# Rsquared: This is a measure of the correlation between the predictions made by the model and the actual observations. 
# The higher the R-squared, the more closely a model can predict the actual observations.
# MAE: The mean absolute error. This is the average absolute difference between the predictions made by the model and 
# the actual observations. The lower the MAE, the more closely a model can predict the actual observations.

model$finalModel
model$resample
sd(model$resample$Rsquared)
###################################################################################
#############################missing values ersetzen
library(mice)
PD.klasse <- as.data.frame(subset(swe, select=c(G, N, L, FL, SP, SC,DSC3)))
PD.klasse$DSC3<-as.factor(PD.klasse$DSC3) 

mice_mod <- mice(PD.klasse, method='rf')
mice_complete <- complete(mice_mod)
PD.klasse$G <- mice_complete$G
PD.klasse$L <- mice_complete$L
PD.klasse$FL <- mice_complete$FL
PD.klasse$SP<- mice_complete$SP
PD.klasse$SC <- mice_complete$SC
set.seed(1234)
PD.klasse <- PD.klasse[complete.cases(PD.klasse),]


indxTrain <- createDataPartition(y = PD.klasse$DSC3,p = 0.75,list = FALSE)
training <- PD.klasse[indxTrain,]
testing <- PD.klasse[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100


#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-6]
y = training$DSC3

fit = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
fit
prediction <- predict(fit,newdata = testing )
confusionMatrix(prediction, testing$DSC3 )
plot(fit)

=======
# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("mlr")
install.packages("naivebayes")
install.packages("rsample")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(klaR)

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(rsample)  # data splitting 

swe2 <- as.data.frame  (subset(swe, select=c(G, L, N, FL, SP, SC, Fehler, Dauer, Seiten, DS, DS2, DS3)))
shuffle_index <- sample(1:nrow(swe2))
swe2 <- swe2[shuffle_index, ]
swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS3)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, DS3)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, FL, SP, SC, DS)))
PD.klasse$DS3<-as.factor(PD.klasse$DS3) 
PD.klasse[1:4]<-round(PD.klasse[1:4],1)
#PD.klasse[1:7]<-round(PD.klasse[1:7],1)



sample_data <- sample.split(PD.klasse, SplitRatio = 0.8)
train <- subset(PD.klasse, sample_data == TRUE)
test <- subset(PD.klasse, sample_data == FALSE)
prop.table(table(train$DS3))
prop.table(table(test$DS3))


# Feature Scaling
#train_scale <- scale(train[, 1:6])
#test_scale <- scale(test[, 1:6])

classifier<-naiveBayes(PD.klasse[,1:4], PD.klasse[,5]) 
table(predict(classifier, PD.klasse[,-5]), PD.klasse[,5])

# Fitting Naive DSC3 Model to training dataset
fit <- naiveBayes(DS3 ~ ., data = train)
fit
# Predicting on test data'
predictions <- predict(fit, newdata = test)
# Confusion Matrix
cm <- table(test$DS3, predictions)
cm
# Model Evaluation
confusionMatrix(cm)


table_mat <- table(predictions, test$DS3)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

###########

fit <- naive_bayes(DS3 ~ ., data = train) 
fit
plot(fit) 

predictions <- predict(fit, train, type = 'prob')
head(cbind(predictions, train))


p1 <- predict(fit, train)
(tab1 <- table(p1, train$DS3))
1 - sum(diag(tab1)) / sum(tab1)

p2 <- predict(fit, test)
(tab2 <- table(p2, test$DS3))
1 - sum(diag(tab2)) / sum(tab2)

#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(PD.klasse)), PD.klasse$DS3) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
PD.sub_dataset=PD.klasse[repeating_sequence,]
#We no longer need the frequency, drop the feature
PD.sub_dataset$DS3=NULL




#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(DS3 ~., data=train)
#What does the model say? Print the model summary
Naive_Bayes_Model


#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,test)
#Confusion matrix to check accuracy
table(NB_Predictions,test$DS3)



train %>%
  filter(DS3 == "zu") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "kein") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>%
  filter(DS3 == "ab") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>% 
  select(FL, SP, SC, G) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

features <- setdiff(names(train), "DSC3")
x <- train[, features]
y <- train$DS3


# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

confusionMatrix(nb.m1)


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

plot(nb.m2)
confusionMatrix(nb.m2)
pred <- predict(nb.m2, newdata = test)




#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)
#fit a regression model and use k-fold CV to evaluate performance
model <-caret:: train(DS ~ ., data = PD.klasse, method = "lm", trControl = ctrl)
#view summary of k-fold CV               
print(model)

# RMSE: The root mean squared error. This measures the average difference between the predictions made by the model 
# and the actual observations. The lower the RMSE, the more closely a model can predict the actual observations.
# Rsquared: This is a measure of the correlation between the predictions made by the model and the actual observations. 
# The higher the R-squared, the more closely a model can predict the actual observations.
# MAE: The mean absolute error. This is the average absolute difference between the predictions made by the model and 
# the actual observations. The lower the MAE, the more closely a model can predict the actual observations.

model$finalModel
model$resample
sd(model$resample$Rsquared)
###################################################################################
#############################missing values ersetzen
library(mice)
PD.klasse <- as.data.frame(subset(swe, select=c(G, N, L, FL, SP, SC,DSC3)))
PD.klasse$DSC3<-as.factor(PD.klasse$DSC3) 

mice_mod <- mice(PD.klasse, method='rf')
mice_complete <- complete(mice_mod)
PD.klasse$G <- mice_complete$G
PD.klasse$L <- mice_complete$L
PD.klasse$FL <- mice_complete$FL
PD.klasse$SP<- mice_complete$SP
PD.klasse$SC <- mice_complete$SC
set.seed(1234)
PD.klasse <- PD.klasse[complete.cases(PD.klasse),]


indxTrain <- createDataPartition(y = PD.klasse$DSC3,p = 0.75,list = FALSE)
training <- PD.klasse[indxTrain,]
testing <- PD.klasse[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100


#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-6]
y = training$DSC3

fit = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
fit
prediction <- predict(fit,newdata = testing )
confusionMatrix(prediction, testing$DSC3 )
plot(fit)

>>>>>>> .r66
