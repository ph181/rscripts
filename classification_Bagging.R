#https://rpubs.com/kjmazidi/195428
swe <- read_excel("Erhebung.xlsx", sheet = "klassifikationsdaten")

library(devtools)
library(C50)
library(randomForest)
library(RWeka)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(gbm)
library(caret)
library(partykit)
library(reprtree)
library(rpart)


library(caTools)
library(ipred)


prozessdaten <- as.data.frame  (subset(swe, select=c(G, L,  DScS, Fehler, Dauer, Fortschritt, Seiten, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.numklas <- prozessdaten[complete.cases(prozessdaten),]




# DAtensatz aufteilen
sample_data <- sample.split(PD.numklas, SplitRatio = 0.8)
train <- subset(PD.numklas, sample_data == TRUE)
test <- subset(PD.numklas, sample_data == FALSE)
prop.table(table(train$DSc))
prop.table(table(test$DSc))


# BAgging CART Bootstrapped Aggregation (Bagging) is an ensemble method that creates multiple models of the same type from different sub-samples
#of the same dataset. The predictions from each separate model are combined together to provide a superior result. This approach has shown participially
#effective for high-variance methods such as decision trees.
# fit model

#X <- as.data.frame( subset(PD.numklas, select=-DSc))
#Y <- factor( PD.numklas$DSc)
#learn <- cbind(Y, X)

train$DSc<-as.factor(train$DSc) 
test$DSc<-as.factor(test$DSc) 

fit <- bagging(DSc ~., data=train,
               nbagg = 150,   
               coob = TRUE,
               control = rpart.control(minsplit = 2, cp = 0))
fit
# summarize the fit
summary(fit)
rpart.plot(fit, 
           extra = 100)


# make predictions
predictions <- predict(fit, test, type="class")
# summarize accuracy
table_mat <- table(predictions, test$DSc)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


cls <- predict(fit, newdata=X)
cat("Misclass error est: ", mean(Y != cls), "")
cat("Misclass error oob: ", fit$err, "")



