#neuronale netze

# Installieren und laden Sie die benötigte Bibliothek
install.packages("neuralnet")
library(neuralnet)

# Erstellen Sie das neuronale Netzwerk
nn_model <- neuralnet(y ~ x1 + x2, data = train_data, hidden = c(5, 3))

# Vorhersagen mit dem Modell
predictions <- compute(nn_model, test_data[, c("x1", "x2")])
