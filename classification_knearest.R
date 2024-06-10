
fragebogen <- as.data.frame  (subset(swe, select=c(G, N, FL, SP, SC, DS)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.num <- fragebogen[complete.cases(fragebogen),]


ran <- sample(1:nrow(PD.num), 0.7 * nrow(PD.num)) 


##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
PD.normklas <- as.data.frame(lapply(PD.num[,c(1,2,3,4,5)], nor))


train <- PD.normklas[ran,] ##extract testing set
test <- PD.normklas[-ran,]  ##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
target_category <- PD.num[ran,6] ##extract 5th column if test dataset to measure the accuracy
test_category <- PD.num[-ran,6]##load the package class
library(class) 
##run knn function
pr <- knn(train,test,cl=target_category,k=13)


##create confusion matrix
tab <- table(pr,test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# In the iris dataset that is already available in R, I have run the k-nearest neighbor algorithm that gave me 80% accurate result.
# First, I normalized the data to convert petal.length, sepal.length, petal.width and sepal.length into a standardized 0-to-1 form 
# so that we can fit them into one box (one graph) and also because our main objective is to predict whether a flower is virginica,
# Versicolor, or setosa and that is why I excluded the column 5 and stored it into another variable called iris_target_category. 
# Then, I separated the normalized values into training and testing dataset. Imagine it this way, that the values from training dataset 
# are firstly drawn on a graph and after we run knn function with all the necessary arguments, we introduce testing dataset’s values 
# into the graph and calculate Euclidean distance with each and every already stored point in graph. Now, although we know which flower 
# it is in testing dataset, we still predict the values and store them in variable called ‘pr’ so that we can compare predicted values 
# with original testing dataset’s values. This way we understand the accuracy of our model and if we are to get new 50 values in future
# and we are asked to predict the category of those 50 values, we can do that with this model.

##store it as data frame
dia <- data.frame(PD.num)

##create a random number equal 90% of total number of rows
ran <- sample(1:nrow(dia),0.9 * nrow(dia))

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##normalization function is created
dia_nor <- as.data.frame(lapply(dia[,c(1,2,3,4,5)], nor))

##training dataset extracted
dia_train <- dia_nor[ran,]

##test dataset extracted
dia_test <- dia_nor[-ran,]##the 2nd column of training dataset because that is what we need to predict about testing dataset
##also convert ordered factor to normal factor
dia_target <- as.factor(PD.num[ran,6])

##the actual values of 2nd couln of testing dataset to compaire it with values that will be predicted
##also convert ordered factor to normal factor
test_target <- as.factor(PD.num[-ran,6])



##run knn function
library(class)
pr <- knn(dia_train,dia_test,cl=dia_target,k=20)

##create the confucion matrix
tb <- table(pr,test_target)

##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)## [1] 71.09752




########################
default_index = sample(nrow(PD.num), 100)
default_train = PD.num[default_index, ]
default_test = PD.num[-default_index, ]
# training data
X_default_train = default_train[, -1]
y_default_train = default_train$DSc

# testing data
X_default_test = default_test[, -1]
y_default_test = default_test$DSc

head(knn(train = X_default_train, 
         test = X_default_test, 
         cl = y_default_train, 
         k = 3),
     n = 25)


accuracy = function(actual, predicted) {
  mean(actual == predicted)
}


accuracy(actual = y_default_test,
         predicted = knn(train = scale(X_default_train), 
                         test = scale(X_default_test), 
                         cl = y_default_train, k = 5))


set.seed(42)
k_to_try = 1:100
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn(train = scale(X_default_train), 
             test = scale(X_default_test), 
             cl = y_default_train, 
             k = k_to_try[i])
  acc_k[i] = accuracy(y_default_test, pred)
}

ex_seq = seq(from = 1, to = 100, by = 5)
seq_along(ex_seq)
ex_storage = rep(x = 0, times = length(ex_seq))
for(i in seq_along(ex_seq)) {
  ex_storage[i] = mean(rnorm(n = 10, mean = ex_seq[i], sd = 1))
}

ex_storage

# plot accuracy vs choice of k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification accuracy",
     main = "Accuracy vs Neighbors")
# add lines indicating k with best accuracy
abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
# add line for max accuracy seen
abline(h = max(acc_k), col = "grey", lty = 2)
# add line for prevalence in test set
abline(h = mean(y_default_test == "No"), col = "grey", lty = 2)


max(acc_k)
max(which(acc_k == max(acc_k)))

mean(y_default_test == "No")
##########################################################################################

