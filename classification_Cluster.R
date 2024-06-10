library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(C50)
library(RWeka)


PD.klasse <- as.data.frame  (subset(swe, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G,Fehler, Dauer, Seiten, Fortschritt)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G,FL, SP, SC, DS)))
PD.klasse <- na.omit(PD.klasse)



shapiro.test(PD.klasse$G)
shapiro.test(PD.klasse$Fehler)
shapiro.test(PD.klasse$Dauer)
shapiro.test(PD.klasse$Seiten)
shapiro.test(PD.klasse$FL)
shapiro.test(PD.klasse$SP)
shapiro.test(PD.klasse$SC)
shapiro.test(PD.klasse$DS)


#Normalisierung
z <- PD.klasse
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale = sds)
#Distanzmatrix
distance = dist(nor)

#Skalierungsvarianten
#PD.scale <- as.data.frame(scale(PD.num))

#prüfen auf Clusterbarkeit
get_clust_tendency(nor,n=nrow(nor)-1)


fviz_dist(distance, lab_size = 8)+labs(title="Fragebogen")
# In the plot above, similar objects are close to one another. 
# Red color corresponds to small distance and blue color indicates big distance between observation.

#optimal clusters
nb <- NbClust(nor, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)
suppressWarnings(fviz_nbclust(nb))


#k-means clustering
set.seed(1234)
kmean <- kmeans(nor, 3, nstart = 25)
kmean
fviz_cluster(kmean, data = nor)


kmean <- eclust(nor, "kmeans", nstart = 25, k = 3)
fviz_silhouette(kmean)
kmean

# making cluster as factor
kmean$cluster <- as.factor(kmean$cluster)
# assining cluster to the original  data set
data.clust.kmean <- cbind(PD.num, cluster = kmean$cluster)
data.clust.kmean
# aggregating the feature by cluster
aggar.kmean <- aggregate(data.clust.kmean[,1:8], by=list(data.clust.kmean$cluster), mean) %>% as.data.frame()
aggar.kmean%>%kable()
set.seed(123)

PD.klasse$Cluster <- kmean$cluster
# Compute k-means with k = 3
# visualising k-means result
fviz_cluster(kmean, data = nor,
             palette = c( "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())# Gap statistic

nuee <- as.data.frame  (subset(swe, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS2)))
nuee <- na.omit(nuee)
nuee$Cluster <- kmean$cluster

neu <- as.data.frame  (subset(nuee, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, Cluster, DS2)))
neu <- as.data.frame  (subset(nuee, select=c(G,Fehler, Dauer, Seiten, DSC2)))
neu <- as.data.frame  (subset(nuee, select=c(G,FL, SP, SC,  Cluster, DSC2)))
neu$DSC2<-as.factor(neu$DSC2) 

neu <- as.data.frame  (subset(neu,Cluster == 1))
neu <- as.data.frame  (subset(neu,Cluster == 2))
neu <- as.data.frame  (subset(neu,Cluster == 3))

neu[1:4]<-round(neu[1:4],1)
neu[1:7]<-round(neu[1:7],1)

#use 70% of dataset as training set and 30% as test set

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(neu), replace=TRUE, prob=c(0.7,0.3))
train  <- neu[sample, ]
test   <- neu[!sample, ]

prop.table(table(train$DSC2))
prop.table(table(test$DSC2))


#C4.5 The C4.5 algorithm is an extension of the ID3 algorithm and constructs a decision tree to maximize information gain (difference in entropy).

fit <- J48(DSC2~., data=train )

# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, test)

# summarize accuracy
if(require("partykit", quietly = TRUE)) plot(fit)
summary(predictions)
table_mat <- table(predictions, test$DSC2)
table_mat <- table(test$DSC2, predictions)

summary(table_mat)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


eval_j48 <- evaluate_Weka_classifier(fit, numFolds = 10, complexity = TRUE, 
                                     seed = 1, class = TRUE)
eval_j48

