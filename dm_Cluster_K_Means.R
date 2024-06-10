library(hopkins)
library(factoextra)
library(NbClust)
library(dplyr)
library(cluster)
library(knitr)


swe2 <- as.data.frame  (subset(swe, select=c(G, L, N, FL, SP, SC, Fehler, Dauer, Seiten, Erstes, DS, DS2, DS3)))

swe2 <- swe2[complete.cases(swe2),]

#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fortschritt, N, L, Fehler, Dauer, Seiten, FL, SP, SC, DS))) #bester HOpkins
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Erstes, Fehler, Dauer, Seiten, FL, SP, SC, DS))) #perfekte 3 CLuster

#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten)))
PD.klasse <- as.data.frame  (subset(swe2, select=c(G,FL, SP, SC, DS)))
PD.klasse<-round(PD.klasse,2)

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

#fviz_dist(distance, lab_size = 8)+labs(title="Fragebogen")
# In the plot above, similar objects are close to one another. 
# Red color corresponds to small distance and blue color indicates big distance between observation.

#hopkins(PD.klasse)

#optimal clusters
nb <- NbClust(nor, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)
suppressWarnings(fviz_nbclust(nb))
fviz_nbclust(nor, kmeans, nstart = 25, method = "wss", nboot = 50)+labs(subtitle = "withing group sum of squares")
fviz_nbclust(nor, kmeans, nstart = 25, method = "silhouette", nboot = 50)+labs(subtitle = "Elbow method") + geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(nor, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+labs(subtitle = "Gap statistic method")

wss<-(nrow(nor)-1)*sum(apply(nor,2,var))
for(i in 1:15) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:15, wss, type = "b", xlab = "anz Cluster", ylab = "withing group sum of squares")

gap_stat <- clusGap(nor, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)



#k-means clustering
set.seed(1234)
kmean <- kmeans(nor, 3, nstart = 25)
kmean
fviz_cluster(kmean, data = nor)

#clusGap(nor,3)


kmean <- eclust(nor, "kmeans", nstart = 25, k = 3)
fviz_silhouette(kmean)
kmean

kmean$betweenss
kmean$totss
kmean$tot.withinss
kmeans$withinss


# making cluster as factor
kmean$cluster <- as.factor(kmean$cluster)
# assining cluster to the original  data set
data.clust.kmean <- cbind(PD.klasse, cluster = kmean$cluster)
data.clust.kmean
# aggregating the feature by cluster
aggar.kmean <- aggregate(data.clust.kmean[,1:4], by=list(data.clust.kmean$cluster), mean) %>% as.data.frame()
aggar.kmean%>%kable()
set.seed(123)

swe2$Cluster <- kmean$cluster
# Compute k-means with k = 3
# visualising k-means result
fviz_cluster(kmean, data = nor,
             palette = c( "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())# Gap statistic



save.image("~/R/Rskripte/trunk/clust.RData")

