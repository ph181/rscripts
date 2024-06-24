#zeitreihenmodelle

# Installieren und laden Sie die benötigte Bibliothek
install.packages("forecast")
library(forecast)

# Erstellen Sie das ARIMA-Modell
arima_model <- auto.arima(train_data)

# Vorhersagen mit dem Modell
predictions <- forecast(arima_model, h = length(test_data))
