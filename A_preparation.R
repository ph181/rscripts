#Daten Vorbereitung


mydata <- Erhebung
mydata <- pmm
mydata <- cart
mydata <- lasso


#subsets bilden

mydata <- as.data.frame(subset(mydata, select=c(A, S, L, G, FL, SP, SC, FV, FN, DF, SV, SN, DS, Fehler, Erstes, Dauer, Fortschritt, Seiten)))



mydata <- mydata[complete.cases(mydata),]

shuffle_index <- sample(1:nrow(mydata))
mydata <- mydata[shuffle_index, ]


arg <- Erhebung[which(Erhebung$L ==0),]


#Kategorien bilden
test_vor <- mydata$FV
test_nach <- mydata$FN

test_vor <- mydata$SV
test_nach <- mydata$SN

differenz <- test_nach - test_vor


kategorie <- ifelse(differenz > 0, "mehr", "weniger")

kategorie <- ifelse(differenz > 0, "1", "0")



data.frame(test_vor, test_nach, differenz, kategorie)

mydata$differenz <- differenz
mydata$kategorie <- kategorie



# Überprüfen, welche Spalten nicht-numerische Daten enthalten
nicht_numerisch <- sapply(mydata, function(x) !is.numeric(x))
print(names(mydata)[nicht_numerisch])

mydata$var <- as.numeric(mydata$var)


PD.num<-round(PD.num,2)

