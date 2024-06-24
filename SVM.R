#SVM


# Installieren und laden Sie die benötigte Bibliothek
install.packages("e1071")
library(e1071)

# Erstellen Sie das SVM-Modell
svm_model <- svm(y ~ ., data = train_data, kernel = "radial")

# Vorhersagen mit dem Modell
predictions <- predict(svm_model, newdata = test_data)









