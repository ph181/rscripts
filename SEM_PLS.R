install_github("gastonstat/plspm")
library(devtools)
install.packages("devtools")
library(mosaic)

library(plspm)

data <- subset(mydata, select=c(SP, SC, SV, FL, SN))

data <- data[complete.cases(data),]

r1 = c(0, 0, 0, 0, 0)

r2 = c(0, 0, 0, 0, 0)

r3 = c(1, 1, 0, 0, 0)

r4 = c(1, 1, 1, 0, 0)

r5 = c(1, 1, 1, 1, 0)
# 
# r6 = c(1, 1, 1, 1, 1, 0, 0)
# 
# r7 = c(1, 1, 1, 1, 1, 1, 0)

path = rbind(r1, r2, r3, r4, r5)

rownames(path) = c("SP", "SC", "SV", "FL", "SN")

colnames(path) = rownames(path)

blocks = list(7:13, 15:21, 23:27, 65,66:70)

blocks = list(1, 2, 3, 4, 5)

innerplot(path)
t(colnames(data))
modes = rep("A", 5)
pls = plspm(data, path, blocks, modes = modes)
summary(pls)

data$Geschlecht<-as.factor(data$Geschlecht)
library(dplyr)
mga<-data%>%
  select(Geschlecht)%>%
  droplevels()

#Anschließend kann über das Bootstrapping ein Gruppenvergleich der Pfadkoeffizienten vorgenommen werden:

mga_boot = plspm.groups(pls, mga$Geschlecht, method = "bootstrap")

#Anschließend können die Unterschiede zwischen den Pfadkoeffizienten der beiden Teilmodelle analysiert und interpretiert werden:

mga_boot$test
