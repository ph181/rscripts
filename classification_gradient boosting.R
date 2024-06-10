###Gradient Boosting

install.packages(c("xgboost", "caTools", "caret"))

library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(gbm)


prozessdaten <- as.data.frame  (subset(swe, select=c(G, L, DScS, Fehlerc, Dauerc, Fortschrittc, Seitenc, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.klasse <- prozessdaten[complete.cases(prozessdaten),]

prozessdaten <- as.data.frame  (subset(swe, select=c(G,   L, DS, Fehler, Dauer, Fortschritt, Seiten, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.num <- prozessdaten[complete.cases(prozessdaten),]
PD.num<-round(PD.num,2)

prozessdaten <- as.data.frame  (subset(swe, select=c(G, L,  DScS, Fehler, Dauer, Fortschritt, Seiten, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.numklas <- prozessdaten[complete.cases(prozessdaten),]



fragebogen <- as.data.frame  (subset(swe, select=c(L, G, N, FL, SP, SC, DScS)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.klasse <- fragebogen[complete.cases(fragebogen),]

fragebogen <- as.data.frame  (subset(swe, select=c(L, G, N, FL, SP, SC, DS)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.num <- fragebogen[complete.cases(fragebogen),]
PD.num<-round(PD.num,2)

sample_data <- sample.split(PD.num, SplitRatio = 0.8)
train <- subset(PD.num, sample_data == TRUE)
test <- subset(PD.num, sample_data == FALSE)
prop.table(table(train$DScS))
prop.table(table(test$DScS))



fragebogen <- as.data.frame  (subset(swe, select=c(L, G, Nc, FLc, SPc, SCc, DSc)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.numklas <- fragebogen[complete.cases(fragebogen),]


set.seed(4212)

PD.numklas$DScS<-as.factor(PD.numklas$DScS) 
PD.klasse$DScS<-as.numeric(levels(PD.klasse$DScS))[PD.klasse$DScS]
PD.numklas$DSc<-as.numeric(levels(PD.numklas$DSc))[PD.numklas$DSc]


sample_data <- sample.split(PD.klasse, SplitRatio = 0.8)
train <- subset(PD.klasse, sample_data == TRUE)
test <- subset(PD.klasse, sample_data == FALSE)
prop.table(table(train$DScS))
prop.table(table(test$DScS))



y_train <- as.integer(train$DScS) - 1
y_test <- as.integer(test$DScS) - 1
X_train <- train %>% select(-DScS)
X_test <- test %>% select(-DScS)



xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

xgb_params <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class =6
)



xgbcv <- xgb.cv(params = xgb_params, 
                data = xgb_train, 
                nrounds = 100, 
                nfold = 5, 
                showsd = TRUE, 
                stratified = TRUE, 
                print_every_n = 10, 
                early_stop_round = 20, 
                maximize = FALSE, 
                prediction = TRUE
                )


xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)



xgb_model


###predicitons
xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(PD.klasse$DSc)
xgb_preds

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(PD.klasse$DSc)[y_test + 1]
xgb_preds

accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))


#Gradient Boosted Machine
#Boosting is an ensemble method developed for classification for reducing bias where models are added to learn the misclassification errors in existing models. 
#It has been generalized and adapted in the form of Gradient Boosted Machines (GBM) for use with CART decision trees for classification and regression



fit <- gbm(DS~., data=PD.num, 
           distribution="multinomial"
)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PD.num)
# summarize accuracy
table(predictions, PD.num$DS)
plot(fit, trial = 2, subtree = NULL)

